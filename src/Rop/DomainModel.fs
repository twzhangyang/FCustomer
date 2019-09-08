module FCustomer.DomainModel
open FCustomer.DomainPrimitiveTypes

type PersonName = {
    FirstName: String10.T
    LastName: String10.T
}

type Customer = {
    Id: CustomerId.T
    Name:PersonName
    Email: EmailAddress.T
}

type DomainMessage =
    | CustomerIsRequired
    | CustomerIdMustBePositive
    | FirstNameIsRequired
    | FirstNameMustNotBeMoreThan10Chars
    | LastNameIsRequired
    | LastNameMustNotBeMoreThan10Chars
    | EmailIsRequired
    | EmailMustNotBeMoreThan20Chars
    | EmailMustContainAtSign
    | EmailAddressChanged of string * string
    | CustomerNotFound
    | SqlCustomerIsINvalid
    | DatabaseTimeout
    | DatabaseError of string
    
let createFirstName firstName =
    let map = function
        | StringError.Missing -> FirstNameIsRequired
        | MustNotBeLongerThan _ -> FirstNameMustNotBeMoreThan10Chars
        | DoesNotMatchPattern _ -> failwithf "not expecting does not match pattern for firstName"
    
    String10.create firstName |> Result.mapMessageR map
    
let createLastName lastName =
    let map = function
    | StringError.Missing -> LastNameIsRequired
    | MustNotBeLongerThan _ -> LastNameMustNotBeMoreThan10Chars
    | DoesNotMatchPattern _ -> failwithf "Not expecting"
    
    String10.create lastName |> FCustomer.Result.mapMessageR map

let createEmail email =
    let map = function
        | StringError.Missing -> EmailIsRequired
        | MustNotBeLongerThan _ -> EmailMustNotBeMoreThan20Chars
        | DoesNotMatchPattern _ -> EmailMustContainAtSign
        
    EmailAddress.create email |> FCustomer.Result.mapMessageR map
    
let createCustomerId customerId =
    let map = function
        | IntegerError.Missing -> CustomerIsRequired
        | MustBePositiveInteger _ -> CustomerIdMustBePositive
        
    CustomerId.create customerId |> FCustomer.Result.mapMessageR map

let createPersonalName firstName lastName =
    {
        FirstName = firstName
        LastName = lastName
    }
    
let createCustomer customerId name email =
    {
        Id = customerId
        Name = name
        Email = email
    }