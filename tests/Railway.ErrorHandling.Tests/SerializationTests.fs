module Railway.ErrorHandling.Tests.SerializationTests
open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Railway.ErrorHandling.ResultA
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

    [<AutoOpen>]
    module String50Helper =
        let create str =
            if String.IsNullOrEmpty str
            then Error("str is empty")
            elif str.Length > 50
            then Error("length should less 50")
            else Ok(String50(str))
        let value (String50 str) = str

    type Birthdate = Birthdate of DateTime

    [<AutoOpen>]
    module BirthdateHelper =
        let create date = Ok(Birthdate(date))
        let value (Birthdate date) = date

    type Person = {
        Name: String50
        Email: String50
        Birthday: Birthdate
    }

module Dto =
    type Person = {
        Name: string
        Email: string
        Birthdate: DateTime
    }

    module Person =
        let fromDomain (person: Domain.Person): Person =
            let name = person.Name |> Domain.String50Helper.value
            let email = person.Email |> Domain.String50Helper.value
            let birthdate = person.Birthday |> Domain.BirthdateHelper.value

            { Name = name; Email = email; Birthdate = birthdate }


        let result = new ResultA.ResultBuilder()

        let createPerson name email birthday =
            { Domain.Name = name; Domain.Email = email; Domain.Birthday = birthday }

        let ToDomain(person: Person) =
            result {
                let! name = person.Name |> Domain.String50Helper.create
                let! email = person.Email |> Domain.String50Helper.create
                let! birthdate = person.Birthdate |> Domain.BirthdateHelper.create

                return { Domain.Name = name; Domain.Email = email; Domain.Birthday = birthdate }
            }
            
        let ToDomain'(person: Person) =
            let (<*>) = ResultA.apply
            let (<!>) = ResultA.map
            
            createPerson <!> Domain.String50Helper.create person.Name
            <*> Domain.String50Helper.create person.Email
            <*> Domain.BirthdateHelper.create person.Birthdate
            
        let ToDomain''(person: Person) =
            ResultA.lift3 createPerson (Domain.String50Helper.create person.Name) (Domain.String50Helper.create person.Email) (Domain.BirthdateHelper.create person.Birthdate)
            
module Json =
    open Newtonsoft
    
    let serialize obj =
        JsonConvert.SerializeObject obj
        
    let deserialize<'a> str =
        try
            JsonConvert.DeserializeObject<'a>(str)
            |> Ok
        with
            | ex -> Error ex
            
    
module Serialize =
    let jsonFromDomain (person : Domain.Person) =
        person
        |> Dto.Person.fromDomain
        |> Json.serialize
        
module Deserialize =
    type DtoError =
        | ValidationError of string
        | DeserializationError of exn
        
    
    let jsonToDomain json : ResultA<Domain.Person, DtoError> =
        let result = new ResultA.ResultBuilder()

        result {
           let! a = json
                    |> Json.deserialize<Dto.Person>
                    |> ResultA.mapError DeserializationError
           let! b = a
                    |> Dto.Person.ToDomain
                    |> ResultA.mapError ValidationError
                    
           return b
        }
        

