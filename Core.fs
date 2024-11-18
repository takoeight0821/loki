namespace Loki

open Name

module Core =
    type Producer =
        | Var of Location * Name
        | Const of Location * Const
        | Mu of Location * Name * Statement

        member this.Location() =
            match this with
            | Var(loc, _) -> loc
            | Const(loc, _) -> loc
            | Mu(loc, _, _) -> loc

        static member var(name, ?loc) =
            let loc = defaultArg loc (Location.FromStackFrame())
            Var(loc, name)

        static member int(value, ?loc) =
            let loc = defaultArg loc (Location.FromStackFrame())
            Const(loc, Int value)

    and Const = Int of int

    and Consumer =
        | Finish of Location
        | Label of Location * Name

        member this.Location() =
            match this with
            | Finish loc -> loc
            | Label(loc, _) -> loc

    and Statement =
        | Prim of Location * string * Producer list * Consumer
        | Switch of Location * Producer * (Const * Statement) list * Statement
        | Cut of Location * Producer * Consumer

        member this.Location() =
            match this with
            | Prim(loc, _, _, _) -> loc
            | Switch(loc, _, _, _) -> loc
            | Cut(loc, _, _) -> loc

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
