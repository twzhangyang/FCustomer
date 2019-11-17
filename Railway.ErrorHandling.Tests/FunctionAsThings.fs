module Railway.ErrorHandling.Tests.FunctionAsThings

let add1 x = 1 + x
let add1' = fun x -> x +1

let add x y = x + y
let add' x = fun y -> x + y
let add'' = fun x y -> x + y
let add''' =
    let inner x y =
        x + y
    inner
