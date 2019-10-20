module FCustomer.Giraffe.Copy.Core

open System
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open FCustomer.Giraffe.Copy.Serialization
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Primitives
open Microsoft.Net.Http.Headers
open System.Threading.Tasks
open FSharp.Control.Tasks.V2.ContextInsensitive

type MissingDependencyException(dependencyName : string) =
    inherit Exception(sprintf "Missing component '%s' in dependency injection container, please register it" dependencyName)
    
type HttpContext with
    member this.GetRequestUrl()=
        this.GetRequestUrl()
        
    member this.GetService<'T>() =
        let t = typeof<'T>
        match this.RequestServices.GetService t with
        | null -> raise (MissingDependencyException t.Name)
        | service -> service :?> 'T
        
    member this.GetLogger<'T>() =
        this.GetService<ILogger<'T>>()
        
    member this.GetHostingEnvironment() =
        this.GetService<IHostingEnvironment>()
        
        
    member this.GetJsonSerializer() : IJsonSerializer =
        this.GetService<IJsonSerializer>()
        
    member this.GetXmlSerializer() : IXmlSerializer =
        this.GetService<IXmlSerializer>()
        
    member this.SetStatusCode (httpStatusCode : int) =
        this.Response.StatusCode <- httpStatusCode
        
    member this.SetHttpHeader (key : string) (value : obj) =
        this.Response.Headers.[key] <- StringValues(value.ToString())
    
    member this.SetContentType (contentType : string) =
        this.SetHttpHeader HeaderNames.ContentType contentType
        
    member this.TryGetRequestHeader (key : string) =
        match this.Request.Headers.TryGetValue key with
        | true, value -> Some (value.ToString())
        | _ -> None
        
    member this.GetRequestHeader (key : string) =
        match this.Request.Headers.TryGetValue key with
        | true, value -> Ok(value.ToString())
        | _ -> Error( sprintf "HTTP request header '%s' is missing" key)
    
    member this.TryGetQueryStringValue (key : string) =
        match this.Request.Query.TryGetValue key with
        | true, value -> Some(value.ToString())
        | _ -> None
        
    member this.GetQueryStringValue (key : string) =
        match this.Request.Query.TryGetValue key with
        | true, value -> Ok(value.ToString())
        | _ -> Error( sprintf "HTTP query string '%s' is missing" key)
        
    member this.GetCookie (key : string) =
        match this.Request.Cookies.TryGetValue key with
        | true, value -> Some(value.ToString())
        | _ -> None
        
    member this.GetFormValue ( key : string) =
        match this.Request.HasFormContentType with
        | false -> None
        | true ->
            match this.Request.Form.TryGetValue key with
            | true, value -> Some(value.ToString())
            | _ -> None
            
            
type HttpFuncResult = Task<HttpContext option>
type HttpFuncResult' = Task<Option<HttpContext>>

type HttpFunc = HttpContext -> HttpFuncResult

type HttpHandler = HttpFunc -> HttpFunc

type ErrorHandler = exn -> ILogger -> HttpHandler

let inline warbler f (next : HttpFunc) (ctx : HttpContext) = f (next, ctx) next ctx

let skipPipeline : HttpFuncResult = Task.FromResult None

let earlyReturn : HttpFunc = Some >> Task.FromResult

let handleContext (contextMap : HttpContext -> HttpFuncResult) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            match! contextMap ctx with
            | Some c ->
                match c.Response.HasStarted with
                | true -> return Some c
                | false -> return! next c
            | None -> return None
        }

let compose (handler1 : HttpHandler) (handler2 : HttpHandler) : HttpHandler =
    fun (final : HttpFunc) ->
        let func = final |> handler2 |> handler1
        fun (ctx : HttpContext ) ->
            match ctx.Response.HasStarted with
            | true -> final ctx
            | false -> func ctx
            
let (>=>) = compose

let rec private chooseHttpFunc (funcs : HttpFunc list) : HttpFunc =
    fun (ctx : HttpContext) ->
        task {
            match funcs with
            | [] -> return None
            | func :: tail ->
                let! result = func ctx
                match result with
                | Some c -> return Some c
                | None -> return! chooseHttpFunc tail ctx
        }

let choose (handlers : HttpHandler list) : HttpHandler =
    fun (next : HttpFunc) ->
        let funcs = handlers |> List.map (fun h -> h next)
        fun (ctx : HttpContext) ->
            chooseHttpFunc funcs ctx

let private httpVerb (validate : string -> bool) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        if validate ctx.Request.Method
        then next ctx
        else skipPipeline

let GET : HttpHandler = httpVerb HttpMethods.IsGet
let POST    : HttpHandler = httpVerb HttpMethods.IsPost
let PUT     : HttpHandler = httpVerb HttpMethods.IsPut
let PATCH   : HttpHandler = httpVerb HttpMethods.IsPatch
let DELETE  : HttpHandler = httpVerb HttpMethods.IsDelete
let HEAD    : HttpHandler = httpVerb HttpMethods.IsHead
let OPTIONS : HttpHandler = httpVerb HttpMethods.IsOptions
let TRACE   : HttpHandler = httpVerb HttpMethods.IsTrace
let CONNECT : HttpHandler = httpVerb HttpMethods.IsConnect

let GET_HEAD : HttpHandler = choose [ GET; HEAD ]

let clearResponse : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        ctx.Response.Clear()
        next ctx

let setStatusCode (statusCode : int) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        ctx.SetStatusCode statusCode
        next ctx
        
let setHttpHeader (key : string) (value : obj) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        ctx.SetHttpHeader key value
        next ctx
        
let mustAccept (mimeTypes : string list) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        let headers = ctx.Request.GetTypedHeaders()
        headers.Accept
        |> Seq.map (fun h -> h.ToString())
        |> Seq.exists ( fun h -> mimeTypes |> Seq.contains h)
        |> function
            | true -> next ctx
            | false -> skipPipeline
            
let redirectTo (permanent : bool) (location : string) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        ctx.Response.Redirect(location, permanent)
        Task.FromResult (Some ctx)


