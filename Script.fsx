open System.Diagnostics

// Location represents a source file location.
[<StructuredFormatDisplay("{FileName}:{Line}:{Column}")>]
type Location =
    { FileName: string
      Line: int
      Column: int }

    static member FromStackFrame() =
        let frame = StackFrame(1, true)

        { FileName = frame.GetFileName()
          Line = frame.GetFileLineNumber()
          Column = frame.GetFileColumnNumber() }

module UniqueGen =
    let mutable private unique = 0

    let Next () =
        unique <- unique + 1
        unique

[<StructuredFormatDisplay("{Value}.{Unique}")>]
type Name =
    { Unique: int
      Value: string }

    static member FromString value =
        { Unique = UniqueGen.Next()
          Value = value }

printfn "%A" (Name.FromString "foo")
printfn "%A" (Name.FromString "bar")

module Core =
    type Producer<'a> =
        | Var of Location * 'a
        | Const of Location * Const
        | Mu of Location * 'a * 'a Statement

        member this.Location() =
            match this with
            | Var(loc, _) -> loc
            | Const(loc, _) -> loc
            | Mu(loc, _, _) -> loc

    and Const = Int of int

    and Consumer<'a> =
        | Finish of Location
        | Label of Location * 'a

        member this.Location() =
            match this with
            | Finish loc -> loc
            | Label(loc, _) -> loc

    and Statement<'a> =
        | Prim of Location * string * 'a Producer list * 'a Consumer
        | Switch of Location * 'a Producer * (Const * 'a Statement) list * 'a Statement
        | Cut of Location * 'a Producer * 'a Consumer

        member this.Location() =
            match this with
            | Prim(loc, _, _, _) -> loc
            | Switch(loc, _, _, _) -> loc
            | Cut(loc, _, _) -> loc

    let var name = Var(Location.FromStackFrame(), name)

    let int value =
        Const(Location.FromStackFrame(), Int value)

    let mu name body =
        Mu(Location.FromStackFrame(), name, body)

    let finish () = Finish(Location.FromStackFrame())

    let label name = Label(Location.FromStackFrame(), name)

    let prim name args =
        let label = Name.FromString name

        Mu(
            Location.FromStackFrame(),
            label,
            Prim(Location.FromStackFrame(), name, args, Label(Location.FromStackFrame(), label))
        )

    let switch prod clauses def =
        let label = Name.FromString "switch"

        let clauses =
            List.map
                (fun (c, p) -> (c, Cut(Location.FromStackFrame(), p, Label(Location.FromStackFrame(), label))))
                clauses

        Mu(
            Location.FromStackFrame(),
            label,
            Switch(
                Location.FromStackFrame(),
                prod,
                clauses,
                Cut(Location.FromStackFrame(), def, Label(Location.FromStackFrame(), label))
            )
        )

    let cut prod cont =
        Cut(Location.FromStackFrame(), prod, cont)

// Eval is a class that evaluates a Core program.
module Eval =
    exception UnknownPrimitive of Location * string
    exception UnexpectedProducer of Location * Core.Producer<Name>
    exception UnexpectedConsumer of Location * Core.Consumer<Name>
    exception UnknownVariable of Location * Name

    type Value = Int of int

    type Covalue = Finish

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
            | ("add", [ Int x; Int y ]) -> apply loc env cont (Int(x + y))
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
            apply loc env covalue value

    and private evalProducer env =
        function
        | Core.Var(loc, name) -> env.Lookup loc name
        | Core.Const(_, Core.Int n) -> Value.Int n
        | p -> raise (UnexpectedProducer(p.Location(), p))

    and private evalConsumer env =
        function
        | Core.Finish _ -> Finish
        | Core.Label(loc, name) -> env.LookupCo loc name

    and private apply (loc: Location) (env: Env) (cont: Covalue) (value: Value) =
        match cont with
        | Covalue.Finish -> value

    let Run (stmt: Core.Statement<Name>) = evalStmt Env.empty stmt

let ex1: Core.Producer<Name> = Core.prim "add" [ Core.int 1; Core.int 2 ]
printfn "%A" ex1


let result1 = Eval.Run(Core.cut ex1 (Core.finish ()))
printfn "%A" result1

let ex2: Core.Producer<Name> =
    Core.switch (Core.int 1) [ Core.Int 1, Core.prim "print" [ Core.int 1 ] ] (Core.int 3)

printfn "%A" ex2

let result2 = Eval.Run(Core.cut ex2 (Core.finish ()))
printfn "%A" result2
