namespace FCustomer.Giraffe.Copy.Serialization
open System.IO
open System.Threading.Tasks
open Utf8Json
open System.Text
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

[<AutoOpen>]
module Json =
    type IJsonSerializer =
        abstract member SerializeToString<'T> : 'T -> string
        abstract member SerializeToBytes<'T> : 'T -> byte array
        abstract member SerializeToStreamAsync<'T> : 'T -> Stream -> Task
        abstract member Deserialize<'T> : string -> 'T
        abstract member Deserialize<'T> : byte[] -> 'T
        abstract member DeserializeAsync<'T> : Stream -> Task<'T>
        
        
    type Utf8JsonSerializer ( resolver : IJsonFormatterResolver) =
        static member DefaultResolver = Utf8Json.Resolvers.StandardResolver.CamelCase
        
        interface IJsonSerializer with
            member __.SerializeToString (x : 'T) =
                JsonSerializer.ToJsonString(x, resolver)
            
            member __.SerializeToBytes (x : 'T) =
                Utf8Json.JsonSerializer.Serialize(x, resolver)
                
            member __.SerializeToStreamAsync ( x : 'T) (stream : Stream) =
                JsonSerializer.SerializeAsync(stream, x)
                
            member __.Deserialize<'T> (json: string) : 'T =
                let bytes = Encoding.UTF8.GetBytes json
                Utf8Json.JsonSerializer.Deserialize(bytes)
                
            member __.Deserialize<'T> (bytes : byte array) : 'T =
                Utf8Json.JsonSerializer.Deserialize(bytes, resolver)
                
            member __.DeserializeAsync<'T> (stream: Stream) : Task<'T> =
                JsonSerializer.DeserializeAsync(stream, resolver)
                
    type NewtonsoftJsonSerializer (settings : JsonSerializerSettings) =
        let Utf8EncodingWithoutBom = new UTF8Encoding(false)
        let DefaultBufferSize = 1024

        static member DefaultSettings = JsonSerializerSettings(ContractResolver = CamelCasePropertyNamesContractResolver())
                
        interface IJsonSerializer with
            member __.SerializeToString(x : 'T) =
                JsonConvert.SerializeObject(x, settings)
                
            member __.SerializeToBytes ( x : 'T) =
                JsonConvert.SerializeObject(x, settings)
                |> Encoding.UTF8.GetBytes
            
            member __.SerializeToStreamAsync ( x : 'T) (stream : Stream) =
                use sw = new StreamWriter(stream, Utf8EncodingWithoutBom, DefaultBufferSize, true)
                use jw = JsonTextWriter(sw)
                let sr = JsonSerializer.Create settings
                sr.Serialize(jw, x)
                Task.CompletedTask
            
            member __.Deserialize<'T> (json : string) =
                JsonConvert.DeserializeObject<'T>(json, settings)
                
            member __.Deserialize<'T> (bytes : byte array) =
                let json = Encoding.UTF8.GetString bytes
                JsonConvert.DeserializeObject<'T>(json, settings)
                
            member __.DeserializeAsync<'T> (stream : Stream) =
                use sr = StreamReader(stream, true)
                use jr = JsonTextReader(sr)
                let sr = JsonSerializer.Create settings
                
                Task.FromResult(sr.Deserialize<'T> jr)
                
[<AutoOpen>]            
module Xml =
    open System.Xml
    open System.Xml.Serialization
    
    type IXmlSerializer =
        abstract member Serialize : obj -> byte array
        abstract member Deserialize<'T> : string -> 'T
        
    type DefaultXmlSerializer (settings : XmlWriterSettings) =
        static member DefaultSettings =
            XmlWriterSettings(
                                 Encoding= Encoding.UTF8,
                                 Indent = true,
                                 OmitXmlDeclaration = false
                             )
        interface IXmlSerializer with
            member __.Serialize (o : obj) =
                use stream = new MemoryStream()
                use writer = XmlWriter.Create(stream, settings)
                let serializer = XmlSerializer(o.GetType())
                serializer.Serialize(writer, o)
                stream.ToArray()

            member __.Deserialize<'T> (xml : string) =
                let serializer = XmlSerializer(typeof<'T>)
                use reader = new StringReader(xml)
                serializer.Deserialize reader :?> 'T
