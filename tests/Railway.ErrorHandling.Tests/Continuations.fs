module Railway.ErrorHandling.Tests.Continuations
open Railway.ErrorHandling.Tests


let dividedBy ifZero ifSuccess top bottom =
    if bottom = 0
    then
        ifZero()
    else
        ifSuccess (top/bottom)
        
let ifZero() = printfn "Bad"
let ifSuccess i = printfn "Good %i" i

let ifZero'() = None
let ifSuccess' i = Some i

let ifZero''() = failwith "div by 0"
let ifSuccess'' i = i
let dividedBy''' = dividedBy ifZero'' ifSuccess''

let pipeInto (someExpression, lambda) =
    printfn "expression is %A" someExpression
    someExpression |> lambda
    
let logger = pipeInto (42, fun x ->
    pipeInto (43, fun y ->
        pipeInto(x + y, fun z ->
            z
            )
        )
    )

        
        