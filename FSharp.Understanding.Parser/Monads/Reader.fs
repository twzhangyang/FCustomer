module FSharp.Understanding.Parser.Reader

type CustomerId = CustomerId of string
type ProductId = ProductId of string
type ProductInfo = {ProductName: string; }

type ApiClient() =
    static let mutable data = Map.empty<string, obj>
    
    member private this.TryCast<'a> key (value:obj) =
        match value with
        | :? 'a as a ->
           Ok a
        | _ ->
            let typeName = typeof<'a>.Name
            Error (sprintf "Can't cast value from %s to %s" key typeName)
            
    member this.Get<'a> (id:obj) =
        let key = sprintf "%A" id
        match Map.tryFind key data with
        | Some value ->
            this.TryCast<'a> key value
        | None ->
            Error (sprintf "Can't get value by key %A" id)
            
    member this.Set<'a> (id:obj) (value:obj) =
        let key = sprintf "%A" id
        if key = "bad" then
            Error(sprintf "Can't set value %A by key %s" value key)
        else
            data <- Map.add key value data
            Ok ()
    
    member this.Open() =
        printfn "Opening"
        
    member this.Close() =
        printfn "Closing"
        
    interface System.IDisposable with
        member this.Dispose() =
            printfn "Disposing"
            
use api = ApiClient()
api.Get "K1" |> printfn "[K1] %A"

api.Set "K2" "hello" |> ignore
api.Get<string> "K2" |> printfn "[K2] %A"

api.Set "K3" "hello" |> ignore
api.Get<int> "K3" |> printfn "[K3] %A"

/// CustId -> ApiClient -> Result<ProductId list>
let getPurchaseIds (custId:CustomerId) (api:ApiClient) =
    api.Get<ProductId list> custId

/// ProductId -> ApiClient -> Result<ProductInfo>
let getProductInfo (productId:ProductId) (api:ApiClient) =
    api.Get<ProductInfo> productId

/// CustId -> ApiClient -> Result<ProductInfo list>
//let getPurchaseInfo (custId:CustomerId) (api:ApiClient) =
//   
//    let result = Result.result {
//        let! productIds = getPurchaseIds custId api 
//
//        let productInfos = ResizeArray()  
//        for productId in productIds do
//            let! productInfo = getProductInfo productId api
//            productInfos.Add productInfo 
//        return productInfos |> List.ofSeq
//        }
//
//    // return result
//    result
//let getPurchaseInfo' = 
//   
//let getPurchaseInfo (custId: CustomerId) =
//    custId 
//    |> getPurchaseIds 
//    |> List.map getProductInfo

type ApiAction<'a> = ApiAction of (ApiClient -> 'a)

let getPurchaseIds' (custId: CustomerId) =
       
    // create the api-consuming function
    let action (api:ApiClient) = 
        api.Get<ProductId list> custId

    // wrap it in the single case
    ApiAction action
    
let getProductInfo' (productId:ProductId) =

    // create the api-consuming function
    let action (api:ApiClient) = 
        api.Get<ProductInfo> productId

    // wrap it in the single case
    ApiAction action

//module ApiAction = 
//
//    /// Evaluate the action with a given api
//    /// ApiClient -> ApiAction<'a> -> 'a
//    let run api (ApiAction action) = 
//        let resultOfAction = action api
//        resultOfAction
//
//    /// ('a -> 'b) -> ApiAction<'a> -> ApiAction<'b>
//    let map f action = 
//        let newAction api =
//            let x = run api action 
//            f x
//        ApiAction newAction
//
//    /// 'a -> ApiAction<'a>
//    let retn x = 
//        let newAction api =
//            x
//        ApiAction newAction
//
//    /// ApiAction<('a -> 'b)> -> ApiAction<'a> -> ApiAction<'b>
//    let apply fAction xAction = 
//        let newAction api =
//            let f = run api fAction 
//            let x = run api xAction 
//            f x
//        ApiAction newAction
//
//    /// ('a -> ApiAction<'b>) -> ApiAction<'a> -> ApiAction<'b>
//    let bind f xAction = 
//        let newAction api =
//            let x = run api xAction 
//            run api (f x)
//        ApiAction newAction
//
//    /// Create an ApiClient and run the action on it
//    /// ApiAction<'a> -> 'a
//    let execute action =
//        use api = new ApiClient()
//        api.Open()
//        let result = run api action
//        api.Close()
//        result
//
//module ApiActionResult = 
//
//    let map f  = 
//        ApiAction.map (Result.map f)
//
//    let retn x = 
//        ApiAction.retn (Result.retn x)
//
//    let apply fActionResult xActionResult = 
//        let newAction api =
//            let fResult = ApiAction.run api fActionResult 
//            let xResult = ApiAction.run api xActionResult 
//            Result.apply fResult xResult 
//        ApiAction newAction
//
//    let bind f xActionResult = 
//        let newAction api =
//            let xResult = ApiAction.run api xActionResult 
//            // create a new action based on what xResult is
//            let yAction = 
//                match xResult with
//                | Success x -> 
//                    // Success? Run the function
//                    f x
//                | Failure err -> 
//                    // Failure? wrap the error in an ApiAction
//                    (Failure err) |> ApiAction.retn
//            ApiAction.run api yAction  
//        ApiAction newAction
//        
//let getPurchaseInfo =
//    let getProductInfoLifted =
//        getProductInfo
//        |> traverse 
//        |> ApiActionResult.bind 
//    getPurchaseIds >> getProductInfoLifted