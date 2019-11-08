namespace Railway.ErrorHandling
open System
open System.Collections.Generic

type Result<'TSuccess, 'TMessage> =
    | OK of 'TSuccess * 'TMessage list
    | Bad of 'TMessage list
    
    static member FailWith (messages : 'TMessage seq) = Bad(messages |> Seq.toList)
    
    static member FailWith (message : 'TMessage) = Bad([message])
    
    static member Succeed (value : 'TSuccess) = Ok(value, [])
    
    static member Succeed (value : 'TSuccess, message : 'TMessage) = Ok(value, [message])
    
    static member Succeed (value : 'TSuccess, messages : 'TMessage seq) = Ok(value, messages |> Seq.toList)
    
//     static member Try(func: Func<'TSuccess>) : Result<'TSuccess,exn> =        
//        try
//            Ok(func.Invoke(),[])
//        with
//        | exn -> Bad[exn]
//
    override x.ToString() =
        match x with
        | OK(v, messages) -> sprintf "%A - %s" v (String.Join(Environment.NewLine, messages |> List.map (fun m -> m.ToString())))
        | Bad(messages) -> sprintf "%s" (String.Join(Environment.NewLine, messages |> List.map (fun m -> m.ToString())))
        
    
module Trail =
    
    let inline ok<'TSuccess, 'TMessage> (x : 'TSuccess) = OK(x, [])
    
    let inline pass<'TSuccess, 'TMessage> (x : 'TSuccess) = OK(x, [])
    
    let inline warn<'TSuccess, 'TMessage> (x : 'TSuccess) (message : 'TMessage) = Ok(x, [message])
    
    let inline fail<'TMessage> (message : 'TMessage) = Bad([message])
    
    let inline failed result = 
        match result with
        | Bad _ -> true
        | _ -> false
    
    // let inline either fSuccess fFailure result =
    //     match result with
    //     | Ok(value, messages) -> fSuccess (value, messages)
    //     | Bad(messages) -> fFailure messages
    
    // let inline returnOrFail result =
    //     let inline raiseExn msgs =
    //         msgs
    //         |> Seq.map (sprintf "%O")
    //         |> String.concat (Environment.NewLine + "\t")
    //         |> failwith
    //     either fst raiseExn result
    
    // let inline mergeMessages messages2 result=
    //     let inline fSuccess (value, messages) = Ok(value, messages @ messages2)
    //     let inline fFailure (messages) = Bad(messages @ messages2)
    //     either fSuccess fFailure result
        
    // let inline bind f result =
    //     let inline fSuccess (value, messages) = f value |> mergeMessages messages
    //     let inline fFailure messages = Bad(messages)
        
    //     either fSuccess fFailure result
        
    