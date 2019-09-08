module FCustomer.CustomersControllerHelper
open System.Net
open Microsoft.AspNetCore.Mvc
open FCustomer.DomainModel

type ResponseMessage =
    | NotFound
    | BadRequest of string
    | InternalServerError of string
    | DomainEvent of DomainMessage 
    
let classify msg =
    match msg with
    | CustomerIsRequired
    | CustomerIdMustBePositive
    | FirstNameIsRequired
    | LastNameIsRequired
    | FirstNameMustNotBeMoreThan10Chars
    | LastNameIsRequired
    | LastNameMustNotBeMoreThan10Chars
    | EmailIsRequired
    | EmailMustContainAtSign
    | EmailMustNotBeMoreThan20Chars -> BadRequest (sprintf "%A" msg)
    | EmailAddressChanged _ ->
        DomainEvent msg
    | CustomerNotFound ->
        NotFound
    | SqlCustomerIsINvalid
    | DatabaseTimeout
    | DatabaseError _ ->
        InternalServerError (sprintf "%A" msg)
    
let primayResponse msgs =
    msgs
    |> List.map classify
    |> List.sort
    |> List.head
    
let a = function
| BadRequest s -> Some s
| _ -> None

let badRequestToStr msgs =
    msgs
    |> List.map classify
    |> List.choose (function BadRequest s -> Some s | _ -> None)
//    |> List.choose a
    |> List.map (sprintf "ValidationError : %s")
    |> List.reduce (+)

let domainEventToStr msgs =
    msgs
    |> List.map classify
    |> List.choose (function DomainEvent s -> Some s | _ -> None)
    |> List.map (sprintf "DomainEvent: %A")
    |> List.reduce (+)
    
let toHttpResult (controller: ControllerBase) msgs: IActionResult =
    match primayResponse msgs with
    | NotFound ->
        upcast NotFoundResult()
    | BadRequest _ ->
        let validationMsg = msgs |> badRequestToStr
        upcast controller.BadRequest validationMsg
    | InternalServerError msg ->
        upcast controller.StatusCode((int)HttpStatusCode.InternalServerError)
    | DomainEvent _ ->
        let eventsMsg = domainEventToStr msgs
        upcast controller.Ok eventsMsg
        
