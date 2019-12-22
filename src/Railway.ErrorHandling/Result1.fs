namespace Railway.ErrorHandling.ResultA
open System

type ResultA<'success, 'failed> =
    | Ok of 'success
    | Error of 'failed
    
[<RequireQualifiedAccess>]
module ResultA=
    
    let bimap onSuccess onFailed xR =
        match xR with
        | Ok x -> onSuccess x
        | Error x -> onFailed x
   
    let retn x =
        Ok x
    
    let map f result =
        match result with
        | Ok x -> Ok(f x)
        | Error x -> Error x
        
    let mapError f result =
        match result with
        | Ok x -> Ok x 
        | Error x -> Error (f x)
        
    let bind f result =
        match result with
        | Ok x -> f x
        | Error x -> Error x
        
    let iter (f : _ -> unit) result =
        map f result |> ignore
        
    let apply fR xR =
        match fR, xR with
        | Ok f, Ok x -> Ok (f x)
        | Error f, Ok x -> Error f
        | Ok f, Error x -> Error x
        | Error f, Error x -> Error x
        
    let sequence aListOfResults =
        let (<*>) = apply
        let (<!>) = map
        let cons head tail = head :: tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValueValue = Ok []
        
        List.foldBack consR aListOfResults initialValueValue

    let lift2 f x1 x2 =
       let (<*>) = apply
       let (<!>) = map
       
       f <!> x1 <*> x2
       
    let lift3 f x1 x2 x3 =
        let (<*>) = apply
        let (<!>) = map
        
        f <!> x1 <*> x2 <*> x3
        
    let lift4 f x1 x2 x3 x4 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3 <*> x4

    let lift5 f x1 x2 x3 x4 x5 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3 <*> x4 <*> x5
        
    let bind2 f x1 x2 = lift2 f x1 x2 |> bind id
    
    let bind3 f x1 x2 x3 = lift3 f x1 x2 x3 |> bind id
    
    let isOk x =
        match x with
        | Ok(v) -> true
        | Error v -> false
        
    let isOk' =
        function
            | Ok _ -> true
            | Error _ -> false
            
    let isError x =
        isOk' x |> not
    
    let filter pred =
        function
            | Ok x -> pred x
            | Error x -> true
            
    let ifError defaultVal =
        function
            | Ok x -> x
            | Error x -> defaultVal
            
    let bindOption f result =
        match result with
        | Ok x -> f x |> Some
        | Error _ -> None
        
    let ofOption opt errorVal =
        match opt with
        | Some x -> Ok x
        | None -> Error errorVal
        
    let toOption =
        function
            | Ok x -> Some x
            | Error x -> None
            
    let toErrorOption =
        function
            | Ok x -> None
            | Error x -> Some x
            
    type ResultBuilder() =
        member this.Return x = retn x
        member this.Bind(x, f) = bind f x
        member this.ReturnFrom(x) = x
        member this.Zero() = this.Return ()
        member this.Delay(f) = f
        member this.Run(f) = f()
        member this.While(guard, body) =
            if not (guard())
            then this.Zero()
            else this.Bind( body(), fun () -> this.While(guard, body))
            
        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e
            
        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation()
            
        member this.Using(disposable: System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () ->
                match disposable with
                    | null -> ()
                    | disp -> disp.Dispose())
            
//        member this.For(sequence:seq<_>, body) =
//            this.Using(sequence.GetEnumerator(),fun enum ->
//                this.While(enum.MoveNext,
//                    this.Delay(fun () -> body enum.Current)))       
        
        member this.Combine (a,b) =
            this.Bind(a, fun () -> b())

module Validation =
    type Validation<'Success, 'Failure> =
        ResultA<'Success, 'Failure list>
        
    let map f (x:Validation<_,_>) :Validation<_,_> =
        ResultA.map f x

    let bind f (x:Validation<_,_>) :Validation<_,_> =
        ResultA.bind f x

    /// Apply a Validation<fn> to a Validation<x> applicatively
    let apply (fV:Validation<_,_>) (xV:Validation<_,_>) :Validation<_,_> =
        match fV, xV with
        | Ok f, Ok x -> Ok (f x)
        | Error errs1, Ok _ -> Error errs1
        | Ok _, Error errs2 -> Error errs2
        | Error errs1, Error errs2 -> Error (errs1 @ errs2)

    //-----------------------------------
    // Lifting

    /// Lift a two parameter function to use Validation parameters
    let lift2 f x1 x2 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2

    /// Lift a three parameter function to use Validation parameters
    let lift3 f x1 x2 x3 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3

    /// Lift a four parameter function to use Validation parameters
    let lift4 f x1 x2 x3 x4 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3 <*> x4

    /// Lift a five parameter function to use Validation parameters
    let lift5 f x1 x2 x3 x4 x5 =
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3 <*> x4 <*> x5

//    let sequence (aListOfValidations:Validation<_,_> list) =
//        let (<*>) = map
//        let (<!>) = apply 
//        let cons head tail = head::tail
//        let consR headR tailR = cons <!> headR <*> tailR
//        let initialValue = Ok [] // empty list inside Result
//
//        List.foldBack consR aListOfValidations initialValue

    let ofResult xR :Validation<_,_> =
        xR |> ResultA.mapError List.singleton

//    let toResult (xV:Validation<_,_>) :Result<_,_> =
//        xV    
            
module Customer =
    let createCustomerId id =
        if id < 0 then
            Error "incorrect id"
        else
            Ok id
    
    let createEmail str =
        if String.IsNullOrEmpty str then
            Error "incorrect email"
        else
            Ok str
            
    type CustomerType ={
        Name : string
        Id : int
        Email : string 
    }
    
    let createCustomer id name email =
        {
          Name = name
          Id = id
          Email = email
          }
     
    let (<!>) = ResultA.map
    let (<*>) = ResultA.apply
    
    let (>>=) = ResultA.bind
    
    let createCustomerResultA id email name =
        createCustomer <!> createCustomerId id <*>  Ok name <*> createEmail email
        
    let result = new ResultA.ResultBuilder()
    
    let createCustomerResultM id email name =
        result {
            let! customerId' = createCustomerId id
            let! email' = createEmail email
            let customer = createCustomer customerId' name email'
            return customer
        }
       
        