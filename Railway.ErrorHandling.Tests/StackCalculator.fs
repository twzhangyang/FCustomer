module Railway.ErrorHandling.Tests.StackCalculator

type Stack = Stack of int list

let push x (Stack stack) =
    Stack (x :: stack)
    
let pop (Stack s) =
    match s with
    | [] -> failwith "Stack underflow"
    | x :: xs -> (x, Stack xs)
    
let dup (Stack s) =
    match s with
    | [] -> Stack []
    | x :: xs -> Stack (x::x::xs)
    
let EMPTY =
    Stack []
    
let PRINT (Stack s) =
    match s with
    | [] -> printf "empty stack"
    | x :: xs -> printf "top is %i" x
    Stack(s)
    
let ADD stack =
    let x, stack' = pop stack
    let y, stack'' = pop stack'
    let sum = x + y
    push sum stack''
    
let TIMES stack =
    let x, stack' = pop stack
    let y, stack'' = pop stack'
    let times = x * y
    push times stack''

let ONE stack = push 1 stack
let TWO stack = push 2 stack
let THREE stack = push 3 stack
let FOUR stack = push 4 stack
let SQUARE = dup >> TIMES

let result =
    EMPTY
    |> ONE
    |> TWO
    |> THREE
    |> FOUR
    |> SQUARE
    |> PRINT