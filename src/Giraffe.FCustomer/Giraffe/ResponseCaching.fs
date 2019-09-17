module Giraffe.FCustomer.Giraffe.ResponseCaching
open System
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.ResponseCaching
open Microsoft.Net.Http.Headers
open Giraffe.FCustomer.Core
open Giraffe.FCustomer.Common
open Microsoft.Extensions.Primitives


type CacheDirective =
    | NoCache
    | Public  of TimeSpan
    | Private of TimeSpan
    
let private noCacheHeader = CacheControlHeaderValue(NoCache = true, NoStore = true)

let inline private cacheHeader isPublic duration =
    CacheControlHeaderValue(
        Public = isPublic,
        MaxAge = Nullable duration)
    
let responseCaching (directive       : CacheDirective)
                    (vary            : string option)
                    (varyByQueryKeys : string array option) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->

        let tHeaders = ctx.Response.GetTypedHeaders()
        let headers  = ctx.Response.Headers

        match directive with
        | NoCache ->
            tHeaders.CacheControl           <- noCacheHeader
            headers.[ HeaderNames.Pragma ]  <- StringValues [| "no-cache" |]
            headers.[ HeaderNames.Expires ] <- StringValues [| "-1" |]
        | Public duration  -> tHeaders.CacheControl <- cacheHeader true duration
        | Private duration -> tHeaders.CacheControl <- cacheHeader false duration

        if vary.IsSome then headers.[HeaderNames.Vary] <- StringValues [| vary.Value |]

        if varyByQueryKeys.IsSome then
            let responseCachingFeature = ctx.Features.Get<IResponseCachingFeature>()
            if isNotNull responseCachingFeature then
                responseCachingFeature.VaryByQueryKeys <- varyByQueryKeys.Value

        next ctx