module Railway.ErrorHandling.Tests.CustomerTests
open Railway.ErrorHandling.ResultA

type Request = {
    UserId : int
    Name : string
    Email : string 
}

type ErrorMessage =
    | NameMustNotBeBlank
    | NameMustNotBeLongerThan of int
    | EmailMustNotBeEmpty
    | SmtpServerError of string 

let checkNameNotBlank input =
    if input.Name ="" then
        Error NameMustNotBeBlank 
    else Ok input

let checkName50 input =
    if input.Name.Length > 50 then
        Error (NameMustNotBeLongerThan 50)
    else Ok input
    
let checkEmail input =
    if input.Email = "" then
        Error EmailMustNotBeEmpty
    else Ok input

let validateRequest input =
    input
    |> checkName50
    |> ResultA.bind checkNameNotBlank
    |> ResultA.bind checkEmail
    
let canonicalEmail (input : Request) =
    { input with Email = input.Email.ToLower().Trim()}
    
let canonicalEmailR twoTrackInput =
    twoTrackInput |> ResultA.map canonicalEmail
    
let goodRequest = {
    UserId = 1
    Name = "name"
    Email = "email"
}

let result =
    goodRequest
    |> validateRequest
    |> canonicalEmailR
    
let updateDb (request: Request) =
    printfn "database updated with userId = %i email = %s" request.UserId request.Email
    ()
    
let tee f result =
    f result
    result

let updateDbR twoTrackInput =
    twoTrackInput |> ResultA.map (tee updateDb)

let result' =
    goodRequest
    |> validateRequest
    |> ResultA.map canonicalEmail
    |> ResultA.map (tee updateDb)
    
let sendEmail (request:Request) =
    if request.Email.EndsWith("example.com") then
        failwithf "Can't send email to %s" request.Email
    else
        printfn "Sending email=%s" request.Email
        request // return request for processing by next step
        
let catch exceptionThrowingFunction handler oneTrackInput =
    try
        Ok (exceptionThrowingFunction oneTrackInput)
    with
    | ex ->
        Error (handler ex)

let catchR exceptionThrowingFunction handler twoTrackInput =
    let catch' = catch exceptionThrowingFunction handler
    twoTrackInput |> ResultA.bind catch'
    
let sendEmailR twoTrackInput =
    let handler (ex:exn) = SmtpServerError ex.Message
    
    catchR sendEmail handler twoTrackInput
    
let result'' request=
    goodRequest
    |> validateRequest
    |> ResultA.map canonicalEmail
    |> ResultA.map (tee updateDb)
    |> sendEmailR
    
let loggerR twoTrackInput =
    match twoTrackInput with
    | Ok (request: Request) ->
        printfn "ok result"
    | Error msg ->
        printfn "error result"
    twoTrackInput
    
let result''' request =
    goodRequest
    |> validateRequest
    |> ResultA.map canonicalEmail
    |> ResultA.map (tee updateDb)
    |> sendEmailR
    |> loggerR
    
let translateError_EN err =
    match err with
    | NameMustNotBeBlank ->
        "Name must not be blank"
    | NameMustNotBeLongerThan i ->
        sprintf "Name must not be longer than %i chars" i
    | EmailMustNotBeBlank ->
        "Email must not be blank"
    | SmtpServerError msg ->
        sprintf "SmtpServerError [%s]" msg
        
let returnMessageR translator result =
    match result with
    | Ok obj ->
        sprintf "200 %A" obj
    | Error msg ->
        let errStr = translator msg
        sprintf "400 %s" errStr
        
let resultR =
    goodRequest
    |> validateRequest
    |> ResultA.map canonicalEmail
    |> ResultA.map (tee updateDb)
    |> sendEmailR
    |> loggerR
    |> returnMessageR translateError_EN