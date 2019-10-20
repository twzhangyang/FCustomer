module FCustomer.Giraffe.Copy.ModelValidation
open FCustomer.Giraffe.Copy.Core


type IModelValidation<'T> =
    abstract member Validate : unit -> Result<'T, HttpHandler>

let validateModel<'T when 'T :> IModelValidation<'T>> (f : 'T -> HttpHandler) (model : 'T) : HttpHandler =
    match model.Validate() with
    | Ok _ -> f model
    | Error err -> err