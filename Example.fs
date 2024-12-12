namespace Loki

open Name

module Example =
    open Core

    (*
    let ex1: Core.Producer = prim "add" [ Producer.int 1; Producer.int 2 ]
    printfn "%A" ex1


    let result1 = Eval.Run(cut ex1 (finish ()))
    printfn "%A" result1

    let ex2: Core.Producer =
        switch (Producer.int 1) [ Int 1, prim "print" [ Producer.int 1 ] ] (Producer.int 3)

    printfn "%A" ex2

    let result2 = Eval.Run(cut ex2 (finish ()))
    printfn "%A" result2

    let ex3: Core.Producer =
        let x = Name.FromString "x"
        let' x (Producer.int 1) (prim "print" [ Producer.var x ])

    printfn "%A" ex3

    let result3 = Eval.Run(cut ex3 (finish ()))
    printfn "%A" result3
    *)

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
                [ Int 0, Cut(loc, Const(loc, Int 1), Label(loc, ret)) ],
                Prim(
                    loc,
                    "sub",
                    [ Var(loc, param); Const(loc, Int 1) ],
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
        Eval.RunWith env (Invoke(loc, ex4.Name, [ Producer.int 5 ], [ Finish(loc) ]))

    printfn "%A" result4
