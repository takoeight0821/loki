namespace Loki

open Name

// Eval is a class that evaluates a Core program.
module Eval =
    exception UnknownPrimitive of Location * string
    exception UnexpectedProducer of Location * Core.Producer
    exception UnexpectedConsumer of Location * Core.Consumer
    exception UnknownVariable of Location * Name

    type Value = Int of int

    type Covalue =
        | Finish
        | Mu' of Name * Core.Statement

    type Env =
        { Values: Map<Name, Value>
          Covalues: Map<Name, Covalue> }

        static member empty =
            { Values = Map.empty
              Covalues = Map.empty }

        member this.Lookup loc name =
            match Map.tryFind name this.Values with
            | Some value -> value
            | None -> raise (UnknownVariable(loc, name))

        member this.Add name value =
            { this with
                Values = Map.add name value this.Values }

        member this.LookupCo loc name =
            match Map.tryFind name this.Covalues with
            | Some value -> value
            | None -> raise (UnknownVariable(loc, name))

        member this.AddCo name value =
            { this with
                Covalues = Map.add name value this.Covalues }

    let rec private evalStmt (env: Env) =
        function
        | Core.Prim(loc, name, args, cont) ->
            let args = List.map (evalProducer env) args
            let cont = evalConsumer env cont

            match (name, args) with
            | ("add", [ Int x; Int y ]) -> apply env cont (Int(x + y))
            | ("print", [ Int x ]) ->
                printfn "%d" x
                Int x
            | _ -> raise (UnknownPrimitive(loc, name))
        | Core.Switch(_, prod, clauses, def) ->
            let value = evalProducer env prod

            let rec go clauses =
                match (value, clauses) with
                | (_, []) -> evalStmt env def
                | (Int m, ((Core.Int n, stmt) :: _)) when m = n -> evalStmt env stmt
                | (_, _ :: rest) -> go rest

            go clauses
        | Core.Cut(_, (Core.Mu(_, label, stmt)), cont) ->
            let covalue = evalConsumer env cont
            let env = env.AddCo label covalue
            evalStmt env stmt
        | Core.Cut(loc, prod, cont) ->
            let value = evalProducer env prod
            let covalue = evalConsumer env cont
            apply env covalue value

    and private evalProducer env =
        function
        | Core.Var(loc, name) -> env.Lookup loc name
        | Core.Const(_, Core.Int n) -> Value.Int n
        | p -> raise (UnexpectedProducer(p.Location(), p))

    and private evalConsumer env =
        function
        | Core.Finish _ -> Finish
        | Core.Label(loc, name) -> env.LookupCo loc name
        | Core.Mu'(_, name, stmt) -> Mu'(name, stmt)

    // Apply a value to a continuation.
    and private apply (env: Env) (cont: Covalue) (value: Value) =
        match cont with
        | Finish -> value
        | Mu'(name, stmt) ->
            let env = env.Add name value
            evalStmt env stmt

    let Run (stmt: Core.Statement) = evalStmt Env.empty stmt
