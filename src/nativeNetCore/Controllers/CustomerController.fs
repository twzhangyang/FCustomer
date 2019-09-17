namespace fsharp.web.Controllers
open System.Diagnostics
open Microsoft.AspNetCore.Mvc
open FCustomer.DataAccessLayer
open System
open FCustomer.DomainModel
open FCustomer.Converter
        
[<Route("api/[controller]")>]
[<ApiController>]
type ValuesController (fsDao: ICustomerDao) as this =
    inherit ControllerBase()
    
    let ok value = (OkObjectResult value) :> IActionResult
    let toHttpResult result =
        result |> FCustomer.Result.valueOrDefault (FCustomer.CustomersControllerHelper.toHttpResult this)

    let log format (objs:obj[]) =
        Trace.WriteLine("log" + String.Format(format, objs))
        
    let logSuccessR format result =
        let logSuccess objj = log format [|objj|]
        
        result |> FCustomer.Result.successTee logSuccess
        
    let logFailureR result =
        let logError err = log "Error: {0}" [| sprintf "%A" err |]
        
        result |> FCustomer.Result.failureTee (Seq.iter logError)
    
    let notifyCustomerWhenEmailChangedR =
        let detectEvent = function
            | EmailAddressChanged (oldEmail, newEmail) -> Some (oldEmail, newEmail)
            | _ -> None
        
        let notifyCustomer (oldEmail, newEmail) =
            log "Email changed from {0} to {1}" [|oldEmail, newEmail|]
            
        FCustomer.Result.successTee (fun (_, msgs) ->
            msgs
            |> List.choose detectEvent
            |> List.iter notifyCustomer
            )
    
    [<Route("customerE/{customerId}")>]
    [<HttpGet>]
    member this.GetWithErrorHandling(customerId:int) : IActionResult =
        FCustomer.Result.succeed customerId
        |> logSuccessR "GetWithErrorHandling {0}"
        |> FCustomer.Result.bindR createCustomerId
        |> FCustomer.Result.bindR fsDao.GetById 
        |> FCustomer.Result.mapR DtoConvert.customerToDto 
        |> logFailureR
        |> FCustomer.Result.mapR ok
        |> toHttpResult
        

    [<Route("customerE/{customerId}")>]
    [<HttpPost>]
    member this.Post(customerId:int, [<FromBody>]dto:CustomerDto) =
        dto.Id <- customerId
        
        FCustomer.Result.succeed dto
        |> logSuccessR "POST with {0}"
        |> FCustomer.Result.bindR DtoConvert.dtoToCustomer
        |> FCustomer.Result.bindR fsDao.Upsert
        |> logFailureR
        |> notifyCustomerWhenEmailChangedR
        |> FCustomer.Result.mapR ok
        |> toHttpResult 