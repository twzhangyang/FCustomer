namespace Giraffe.FCustomer.Serialization
open Newtonsoft.Json
open Utf8Json

module Json =
    open Newtonsoft.Json.Serialization
    open System.Text
    open System.Threading.Tasks
    open System.IO
    
    type IJsonSerializer =
        abstract member SerializeToString<'T> : 'T -> string
        abstract member SerializeToBytes<'T> : 'T -> byte array
        abstract member SerializeToStreamAsync<'T> : 'T -> Stream -> Task
        abstract member Deserialize<'T> : string -> 'T
        abstract member Deserialize<'T> : byte[] -> 'T
        abstract member DeserializeAsync<'T> : Stream -> Task<'T>
        
    type Utf8JsonSerializer (resolver : IJsonFormatterResolver) =
        static member DefaultResolver = Utf8Json.Resolvers.StandardResolver.CamelCase
        
        interface IJsonSerializer with
            member __.SerializeToString (x : 'T) =
                JsonSerializer.ToJsonString (x, resolver)
                
            member __.SerializeToBytes (x : 'T) =
                JsonSerializer.Serialize(x)
            
            member __.SerializeToStreamAsync (x : 'T) (stream: Stream) =
                JsonSerializer.SerializeAsync(stream, x, resolver)
                
            member __.Deserialize (x : string) : 'T =
                let bytes = Encoding.UTF8.GetBytes x
                JsonSerializer.Deserialize(bytes)
                
            member __.Deserialize (x : byte[]) : 'T =
                JsonSerializer.Deserialize(x)
            
            member __.DeserializeAsync (stream : Stream) : Task<'T> =
                JsonSerializer.DeserializeAsync(stream, resolver)
    
     type NewtonsoftJsonSerializer (settings : JsonSerializerSettings) =

        let Utf8EncodingWithoutBom = new UTF8Encoding(false)
        let DefaultBufferSize = 1024

        static member DefaultSettings =
           new JsonSerializerSettings(
               ContractResolver = CamelCasePropertyNamesContractResolver())

        interface IJsonSerializer with
            member __.SerializeToString (x : 'T) =
                JsonConvert.SerializeObject(x, settings)

            member __.SerializeToBytes (x : 'T) =
                JsonConvert.SerializeObject(x, settings)
                |> Encoding.UTF8.GetBytes

            member __.SerializeToStreamAsync (x : 'T) (stream : Stream) =
                use sw = new StreamWriter(stream, Utf8EncodingWithoutBom, DefaultBufferSize, true)
                use jw = new JsonTextWriter(sw)
                let sr = JsonSerializer.Create settings
                sr.Serialize(jw, x)
                Task.CompletedTask

            member __.Deserialize<'T> (json : string) =
                JsonConvert.DeserializeObject<'T>(json, settings)

            member __.Deserialize<'T> (bytes : byte array) =
                let json = Encoding.UTF8.GetString bytes
                JsonConvert.DeserializeObject<'T>(json, settings)

            member __.DeserializeAsync<'T> (stream : Stream) =
                use sr = new StreamReader(stream, true)
                use jr = new JsonTextReader(sr)
                let sr = JsonSerializer.Create settings
                Task.FromResult(sr.Deserialize<'T> jr)

