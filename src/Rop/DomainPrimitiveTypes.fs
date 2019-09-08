module FCustomer.DomainPrimitiveTypes
open FCustomer.Result

type StringError =
    | Missing
    | MustNotBeLongerThan of int
    | DoesNotMatchPattern of string
    

type IntegerError =
    | Missing
    | MustBePositiveInteger
    
module String10 =
    type T = String10 of string
    let create (s:string) =
        match s with
        | null -> fail StringError.Missing
        | _ when s.Length > 10 -> fail (MustNotBeLongerThan 10)
        | _ -> succeed (String10 s)
        
    let apply f (String10 s) =
        f s
        
module String20 =
    type T = String20 of string
    let create (s:string) =
        match s with
        | null -> fail StringError.Missing
        | _ when s.Length > 20 -> fail (MustNotBeLongerThan 20)
        | _ -> succeed (String20 s)
        
    let apply f (String20 s) =
        f s
module EmailAddress =
    type T = EmailAddress of String20.T
    
    let create (s:string) =
        match String20.create s with
        | Failure error -> Failure error
        | Success (s20, _) ->
            if s.Contains("@") then
                succeed (EmailAddress s20)
            else
                let err = DoesNotMatchPattern "*"
                fail err
                
    let apply f (EmailAddress s20) =
        let s = s20 |> String20.apply id
        f s
        
module CustomerId =
    open System
    
    type T = CustomerId of int
    let create (i:int) =
        if i < 1 then
            fail MustBePositiveInteger
        else
            succeed (CustomerId i)
    
    let createFromNullable (i: Nullable<int>) =
        if not i.HasValue then
            fail IntegerError.Missing
        else if i.Value < 1 then
             fail MustBePositiveInteger
        else succeed (CustomerId i.Value)
        
    let apply f (CustomerId i) =
        f i
        