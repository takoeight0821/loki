namespace Loki

open Name

module Core =
    type Producer =
        | Var of Location * Name
        | Const of Location * Const
        /// Do binds a variable to a continuation and executes a statement.
        | Do of Location * Name * Statement

        member this.Location() =
            match this with
            | Var(loc, _) -> loc
            | Const(loc, _) -> loc
            | Do(loc, _, _) -> loc

    and Const = Int of int

    and Consumer =
        | Finish of Location
        | Label of Location * Name
        // Then binds a variable to a result and executes a statement.
        | Then of Location * Name * Statement

        member this.Location() =
            match this with
            | Finish loc -> loc
            | Label(loc, _) -> loc
            | Then(loc, _, _) -> loc

    and Statement =
        | Prim of Location * string * Producer list * Consumer
        | Switch of Location * Producer * (Const * Statement) list * Statement
        | Cut of Location * Producer * Consumer
        | Invoke of Location * Name * Producer list * Consumer list

        member this.Location() =
            match this with
            | Prim(loc, _, _, _) -> loc
            | Switch(loc, _, _, _) -> loc
            | Cut(loc, _, _) -> loc
            | Invoke(loc, _, _, _) -> loc

    and Definition =
        { Name: Name
          Params: Name list
          Returns: Name list
          Body: Statement }

    let finish () = Finish(Location.FromStackFrame())


    let let' name term body =
        let label = Name.FromString "let"

        Do(
            Location.FromStackFrame(),
            label,
            Cut(
                Location.FromStackFrame(),
                term,
                Then(
                    Location.FromStackFrame(),
                    name,
                    Cut(Location.FromStackFrame(), body, Label(Location.FromStackFrame(), label))
                )
            )
        )

    let prim name args =
        let label = Name.FromString name

        Do(
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

        Do(
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

    let invoke name args conts =
        Invoke(Location.FromStackFrame(), name, args, conts)
