module FCustomer.Result 

type Result<'TSuccess, 'TMessage> =
    | Success of 'TSuccess * 'TMessage list
    | Failure of 'TMessage list
    
let succeed x =
    Success (x, [])
    
let succeedWithMsg x msg =
    Success (x, [msg])
    
let fail msg =
    Failure [msg]
    
let either fSuccess fFailure = function
    | Success (x, msgs) -> fSuccess (x, msgs)
    | Failure msgs -> fFailure msgs
    
let mergeMessages msgs result =
    let fSuccess (x, msg2) =
        Success (x, msg2 @ msgs)
    let fFailure errs =
        Failure (errs @ msgs)
        
    either fSuccess fFailure result
    
let bindR f result =
    let fSuccess (x, msgs) =
        f x |> mergeMessages msgs
        
    let fFailure errs =
        Failure errs
        
    either fSuccess fFailure result
    
let applyR f result =
    match f, result with
    | Success (f, msg1), Success (x, msg2) ->
        (f x, msg1 @ msg2) |> Success
    | Failure errs, Success (_, msgs)
    | Success (_, msgs), Failure errs ->
        errs @ msgs |> Failure
    | Failure errs1, Failure errs2 ->
        errs1 @ errs2 |> Failure
        
let (<*>) = applyR

let liftR f result =
    let f' = f |> succeed
    
    applyR f' result
    
let lift2R f result1 result2 =
    let f' = liftR f result1
    
    applyR f' result2
    
let lift3R f result1 result2 result3 =
    let f' = lift2R f result1 result2
    
    applyR f' result3

let (<!>) = liftR
let mapR = liftR

let successTee f result =
    let fSuccess (x, msg) =
        f (x, msg)
        Success (x, msg)
    
    let fFailure errs = Failure errs
    either fSuccess fFailure result
        
let failureTee f result =
    let fSuccess (x, msgs) = Success (x, msgs)
    let fFailure errs =
        f errs
        Failure errs
    
    either fSuccess fFailure result

let mapMessageR f result =
    match result with
    | Success (x, msgs) ->
        let msgs' = List.map f msgs
        Success (x, msgs')
    | Failure errs ->
        let errs' = List.map f errs
        Failure errs'
    
let valueOrDefault f result =
    match result with
    | Success (x, _) -> x
    | Failure errs -> f errs
    
let failIfNoneR message = function
    | Some rop -> rop
    | None -> fail message
    
    