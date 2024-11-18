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
        | Mu' of Env * Name * Core.Statement

    and Env =
        { Values: Map<Name, Value> list
          Covalues: Map<Name, Covalue> list
          Toplevels: Map<Name, Core.Definition> }

        static member empty =
            { Values = []
              Covalues = []
              Toplevels = Map.empty }

        member this.Lookup loc name =
            match this.Values with
            | [] -> raise (UnknownVariable(loc, name))
            | values :: rest ->
                match Map.tryFind name values with
                | Some value -> value
                | None -> { this with Values = rest }.Lookup loc name

        member this.Add name value =
            { this with
                Values =
                    match this.Values with
                    | [] -> [ Map.empty.Add(name, value) ]
                    | values :: rest -> Map.add name value values :: rest }

        member this.LookupCo loc name =
            match this.Covalues with
            | [] -> raise (UnknownVariable(loc, name))
            | covalues :: rest ->
                match Map.tryFind name covalues with
                | Some value -> value
                | None -> { this with Covalues = rest }.LookupCo loc name


        member this.AddCo name value =
            { this with
                Covalues =
                    match this.Covalues with
                    | [] -> [ Map.empty.Add(name, value) ]
                    | covalues :: rest -> Map.add name value covalues :: rest }

        member this.LookupToplevel name =
            match Map.tryFind name this.Toplevels with
            | Some value -> value
            | None -> raise (UnknownVariable(Location.FromStackFrame(), name))

        member this.AddToplevel(value: Core.Definition) =
            { this with
                Toplevels = Map.add value.Name value this.Toplevels }

    let rec private evalStmt (env: Env) =
        function
        | Core.Prim(loc, name, args, cont) ->
            let args = List.map (evalProducer env) args
            let cont = evalConsumer env cont

            match (name, args) with
            | ("add", [ Int x; Int y ]) -> apply cont (Int(x + y))
            | ("sub", [ Int x; Int y ]) -> apply cont (Int(x - y))
            | ("mul", [ Int x; Int y ]) -> apply cont (Int(x * y))
            | ("print", [ Int x ]) ->
                printfn "%d" x
                Int x
            | _ -> raise (UnknownPrimitive(loc, name))
        | Core.Switch(loc, prod, clauses, def) ->
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
        | Core.Cut(_, prod, cont) ->
            let value = evalProducer env prod
            let covalue = evalConsumer env cont
            apply covalue value
        | Core.Invoke(_, name, args, conts) ->
            let def = env.LookupToplevel name
            let args = List.map (evalProducer env) args
            let conts = List.map (evalConsumer env) conts

            let env =
                List.fold2 (fun (env: Env) name value -> env.Add name value) env def.Params args

            let env =
                List.fold2 (fun (env: Env) name cont -> env.AddCo name cont) env def.Returns conts

            evalStmt env def.Body



    and private evalProducer env =
        function
        | Core.Var(loc, name) -> env.Lookup loc name
        | Core.Const(loc, Core.Int n) -> Int n
        | p -> raise (UnexpectedProducer(p.Location(), p))

    and private evalConsumer env =
        function
        | Core.Finish loc -> Finish
        | Core.Label(loc, name) -> env.LookupCo loc name
        | Core.Mu'(loc, name, stmt) -> Mu'(env, name, stmt)

    // Apply a value to a continuation.
    and private apply (cont: Covalue) (value: Value) =
        match cont with
        | Finish -> value
        | Mu'(env, name, stmt) ->
            let env = env.Add name value
            evalStmt env stmt

    let Run (stmt: Core.Statement) = evalStmt Env.empty stmt

    let RunWith env (stmt: Core.Statement) = evalStmt env stmt
