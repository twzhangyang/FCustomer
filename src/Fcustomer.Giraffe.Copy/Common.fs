module FCustomer.Giraffe.Copy.Common

open System
open System.IO
open FSharp.Control.Tasks.V2.ContextInsensitive

type DateTime with
    member __.ToHtmlString() = __.ToString("r")
    member __.ToIsoString() = __.ToString("o")

type DateTimeOffset with
    member __.ToHtmlString() = __.ToString("r")
    member __.ToIsoString() = __.ToString("o")

let inline isNotNull x = not (isNull x)

let strOption (x: string) =
    if String.IsNullOrEmpty x then None
    else Some x

let readFileAsStringAsync (filePath: string) =
    task {
        use reader = new StreamReader(filePath)
        return! reader.ReadToEndAsync()
    }

[<RequireQualifiedAccess>]
module ShortGuid =
let fromGuid (guid: Guid) =
  guid.ToByteArray()
  |> Convert.ToBase64String
  |> fun str ->
      str.Replace("/", "_")
         .Replace("+", "-")
         .Substring(0, 22)


let toGuid (shortGuid: string) =
    shortGuid.Replace("_", "/")
             .Replace("-", "+")
    |> (fun str -> str + "==")
    |> Convert.FromBase64String
    |> Guid

[<RequireQualifiedAccess>]
module ShortId =
    let fromUInt64 (id : uint64) =
        BitConverter.GetBytes id
        |> (fun arr ->
            match BitConverter.IsLittleEndian with
            | true  -> Array.Reverse arr; arr
            | false -> arr)
        |> Convert.ToBase64String
        |> (fun str ->
            str.Remove(11, 1)
               .Replace("/", "_")
               .Replace("+", "-"))

    let toUInt64 (shortId : string) =
        let bytes =
            shortId.Replace("_", "/")
                   .Replace("-", "+")
            |> (fun str -> str + "=")
            |> Convert.FromBase64String
            |> (fun arr ->
                match BitConverter.IsLittleEndian with
                | true  -> Array.Reverse arr; arr
                | false -> arr)
        BitConverter.ToUInt64 (bytes, 0)


