module Giraffe.FCustomer.Common
open System
open System.IO
open FSharp.Control.Tasks.V2.ContextInsensitive
    
type DateTime with
    member this.ToHtmlString() = this.ToString("r")
    
type DateTimeOffset with
     member this.ToHtmlString() = this.ToString("r")
     
let inline isNotNull x = not (isNull x)

let inline strOption (str : string) =
    if String.IsNullOrEmpty str then None else Some str
    
let readFileAsStringAsync (filePath : string) =
    task {
        use reader = StreamReader(filePath)
        return! reader.ReadToEndAsync()
    }

[<RequireQualifiedAccess>]    
module ShortGuid =
    let fromGuid (guid : Guid) =
        guid.ToByteArray()
        |> Convert.ToBase64String
        |> (fun str ->
            str.Replace("/", "_")
               .Replace("+", "-")
               .Substring(0, 22)
            )
        
    let toGuid ( shortGuid : string) =
        shortGuid.Replace("_", "/")
                 .Replace("-", "+")
        |> (fun str -> str + "==")
        |> Convert.FromBase64String
        |> Guid
        
module ShortId =
    type unit64 = UInt64
    let fromUInt64 (id : unit64) =
        BitConverter.GetBytes id
        |> (fun arr ->
                match BitConverter.IsLittleEndian with
                | true -> Array.Reverse arr; arr
                | false -> arr
            )
        |> Convert.ToBase64String
        |> (fun str ->
                str.Remove(11,1)
                   .Replace("/", "_")
                   .Replace("+", "-")
            )
        
    let toUInt64 (shortId : string) =
        let bytes =
            shortId.Replace("_", "/")
                   .Replace("-", "+")
            |> (fun str -> str + "==")
            |> Convert.FromBase64String
            |> (fun arr ->
                    match BitConverter.IsLittleEndian with
                    | true -> Array.Reverse arr; arr
                    | false -> arr
                )
        BitConverter.ToUInt64(bytes, 0)
   