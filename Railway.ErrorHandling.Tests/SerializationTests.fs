module Railway.ErrorHandling.Tests.SerializationTests
open System

type WorkflowInput = Input
type WorkflowOutput = Output

type Workflow = WorkflowInput -> WorkflowOutput
type JsonString = string
type InputDto = Dto

type DeserializeInputDto = JsonString -> InputDto
type InputDtoToDomain = InputDto -> WorkflowInput

type OutputDto = OutDto
type DomainToOutputDto = WorkflowOutput -> OutputDto
type SerializeOutputDto = OutputDto -> JsonString

module Domain =
    type String50 = String50 of string
    module String50 =
        let create str =
            if String.IsNullOrEmpty str
            then None
            elif str.Length > 50
            then None
            else Some (String50(str))
        let value (String50 str) = str 
    
    type Birthdate = Birthdate of DateTime
    module Birthdate =
        let create date = Birthdate(date)
        let value birthdate = birthdate
        
    type Person ={
        Name : String50
        Email : String50
        Birthday : Birthdate
    }
    
    
