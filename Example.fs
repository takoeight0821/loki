namespace Loki

open Name

module Example =
    open Core
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
