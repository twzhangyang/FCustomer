module Railway.ErrorHandling.Tests.ValidateRequest
open Railway.ErrorHandling.ResultA
open Railway.ErrorHandling.ResultA
open Xunit

type Input ={
    Name : string
    Email : string 
}

let checkNameNotBlank input =
    if input.Name ="" then
        Error "Name must not be empty"
    else Ok input

let checkName50 input =
    if input.Name.Length > 50 then
        Error "Name length can not great than 50"
    else Ok input
    
let checkEmail input =
    if input.Email = "" then
        Error "Email must not be empty"
    else Ok input

let checkInput input =
    checkNameNotBlank input
    |> ResultA.bind checkName50
    |> ResultA.bind checkEmail

[<Fact>]    
let ``good input should return Ok`` () =
    let input ={
        Name = "hello"
        Email = "test@test.com"
    }
    
    let result = checkInput input
    Assert.True(ResultA.isOk result)

[<Fact>]
let ``empty name should return error`` () =
    let input = {
        Name = ""
        Email = "test@test.com"
    }
    
    let result = checkInput input
    Assert.True(ResultA.isError result)