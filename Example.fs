namespace Loki

open Name

module Example =
    open Core

    let ex1: Core.Producer =
        let loc = Location.FromStackFrame()
        prim "add" [ Const(loc, Int(1)); Const(loc, Int(2)) ]
    printfn "%A" ex1


    let result1 = Eval.Run(cut ex1 (finish ()))
    printfn "%A" result1

    let ex2: Core.Producer =
        let loc = Location.FromStackFrame()
        switch (Const(loc, Int(1))) [ Int 1, prim "print" [ Const(loc, Int(1)) ] ] (Const(loc, Int(3)))

    printfn "%A" ex2

    let result2 = Eval.Run(cut ex2 (finish ()))
    printfn "%A" result2

    let ex3: Core.Producer =
        let x = Name.FromString "x"
        let loc = Location.FromStackFrame()
        let' x (Const(loc, Int(1))) (prim "print" [ Var(loc, x) ])

    printfn "%A" ex3

    let result3 = Eval.Run(cut ex3 (finish ()))
    printfn "%A" result3

    let ex4: Definition =
        let name = Name.FromString "fac"
        let param = Name.FromString "n"
        let ret = Name.FromString "a"
        let loc = Location.FromStackFrame()
        let x = Name.FromString "x"
        let r = Name.FromString "r"

        { Name = name
          Params = [ param ]
          Returns = [ ret ]
          Body =
            Switch(
                loc,
                Var(loc, param),
                [ Int 0, Cut(loc, Const(loc, Int(1)), Label(loc, ret)) ],
                Prim(
                    loc,
                    "sub",
                    [ Var(loc, param); Const(loc, Int(1)) ],
                    Then(
                        loc,
                        x,
                        Invoke(
                            loc,
                            name,
                            [ Var(loc, x) ],
                            [ Then(loc, r, Prim(loc, "mul", [ Var(loc, param); Var(loc, r) ], Label(loc, ret))) ]
                        )
                    )
                )
            ) }

    printfn "%A" ex4

    let result4 =
        let env = Eval.Env.empty.AddToplevel ex4
        let loc = Location.FromStackFrame()
        Eval.RunWith env (Invoke(loc, ex4.Name, [ Const(loc, Int(5)) ], [ Finish(loc) ]))

    printfn "%A" result4
