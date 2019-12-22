module Railway.ErrorHandling.Tests.Builder

type LoggingBuilder() =
    let log p = printfn "expression is %A" p
    
    member this.Bind(x, f) =
        log x
        f x
        
    member this.Return(x) =
        x

let logger = LoggingBuilder()
let loggedWorkflow =
    logger {
        let! x = 42
        let! y = 43
        let! z = x + y
        return z
    }
    
let divideBy bottom top =
    if bottom = 0
    then None
    else Some(top/bottom)
   
   
let divideByWorkflow init x y z =
    let a = init |> divideBy x
    match a with
    | None -> None
    | Some a' ->
        let b = a' |> divideBy y
        match b with
        | None -> None
        | Some b' ->
            let c = b' |> divideBy z
            c
            
type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some a -> f a
        
    member this.Return(x) =
        Some x
        
let maybe = MaybeBuilder()
let divideByWorkflow' init x y z =
    maybe {
        let! a = init |> divideBy x
        let! b = a |> divideBy y
        let! c = b |> divideBy z
        return c
    }
    
let map1 = [("1", "One"); ("2", "Two"); ("3", "Three")] |> Map.ofList
let map2 = [("A", "Alice"); ("B", "Bob")] |> Map.ofList
let map3 = [("CA", "California"); ("NY", "New Y    ork")] |> Map.ofList

let multiLookup key =
    match map1.TryFind key with
    | Some result1 -> Some result1
    | None ->
        match map2.TryFind key with
        | Some result2 -> Some result2
        | None ->
            match map3.TryFind key with
            | Some result3 -> Some result3
            | None -> None

type OrElseBuilder() =
    member this.ReturnFrom(x) = x
    member this.Combine (a, b) =
        match a with
        | Some _ -> a
        | None -> b
        
    member this.Delay(f) = f()
  
let orElse = OrElseBuilder()
let multiLookup' key =
  orElse {
      return! map1.TryFind key
      return! map2.TryFind key
      return! map3.TryFind key
  }
  
open System.Net
let req1 = HttpWebRequest.Create("http://aa.com")
let req2 = HttpWebRequest.Create("http://bb.com")
let req3 = HttpWebRequest.Create("http://cc.com")

req1.BeginGetResponse((fun r1 ->
    use resp1 = req1.EndGetResponse(r1)
    printfn "Downloading %O" resp1.ResponseUri
    
    req2.BeginGetResponse((fun r2 ->
        use resp2 = req2.EndGetResponse(r2)
        printfn "Downloading %O" resp2.ResponseUri
        
        req3.BeginGetResponse((fun r3 ->
            use resp3 = req3.EndGetResponse(r3)
            printfn "Downloading %O" resp3.ResponseUri
            ), null) |> ignore
        ), null) |> ignore
    ), null) |> ignore

async {
    use! resp1 = req1.AsyncGetResponse()
    printfn "Downloading %O" resp1.ResponseUri
    
    use! resp2 = req2.AsyncGetResponse()
    printfn "Downloading %O" resp2.ResponseUri
    
    use! resp3 = req3.AsyncGetResponse()
    printfn "Downloading %O" resp3.ResponseUri
}

type TraceBuilder() =
    member this.Bind(m, f) = 
        match m with 
        | None -> 
            printfn "Binding with None. Exiting."
        | Some a -> 
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m

    member this.Return(x) = 
        printfn "Returning a unwrapped %A as an option" x
        Some x

    member this.ReturnFrom(m) = 
        printfn "Returning an option (%A) directly" m
        m
        
    member this.Zero() = 
        printfn "Zero"
        None
        
    member this.Yield(x) = 
        printfn "Yield an unwrapped %A as an option" x
        Some x


// make an instance of the workflow 
let trace = new TraceBuilder()

trace { 
    return 1
    } |> printfn "Result 1: %A" 

trace { 
    return! Some 2
    } |> printfn "Result 2: %A" 

trace { 
    let! x = Some 1
    let! y = Some 2
    return x + y
    } |> printfn "Result 3: %A" 

trace { 
    let! x = None
    let! y = Some 1
    return x + y
    } |> printfn "Result 4: %A" 

trace { 
    do! Some (printfn "...expression that returns unit")
    do! Some (printfn "...another expression that returns unit")
    let! x = Some (1)
    return x
    } |> printfn "Result from do: %A"

trace {
    printfn "hello world"
    } |> printfn "Result for empty: %A" 
        
trace { 
    yield 1
    } |> printfn "Result for yield: %A"


