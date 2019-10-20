
module FCustomer.Giraffe.Copy.Middleware
open Microsoft.AspNetCore.Http
open FCustomer.Giraffe.Copy.Core
open System
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.DependencyInjection.Extensions
open FCustomer.Giraffe.Copy.Serialization

type GiraffeMiddleware (next : RequestDelegate, handler : HttpHandler, loggerFactory : ILoggerFactory) =
    do
        if isNull next then raise (ArgumentNullException("next"))
        
    let func : HttpFunc = handler ( Some >> Task.FromResult)

    member __.Invoke (ctx : HttpContext) =
        task {
            let start = System.Diagnostics.Stopwatch.GetTimestamp()
            let! result = func ctx
            let logger = loggerFactory.CreateLogger<GiraffeMiddleware>()

            if logger.IsEnabled LogLevel.Debug then 
                let freq = double System.Diagnostics.Stopwatch.Frequency
                let stop = System.Diagnostics.Stopwatch.GetTimestamp()
                let elapsedMs = (double (stop - start)) * 1000.0 / freq

                logger.LogDebug("Giraffe returned {SomeNoneResult} for {HttpProtocol} {HttpMethod} at {Path} in {ElapsedMs}",
                    (if result.IsSome then "Some" else "None"),
                    ctx.Request.Protocol,
                    ctx.Request.Method,
                    ctx.Request.Path.ToString(),
                    elapsedMs)

            if (result.IsNone) then 
                return! next.Invoke ctx
        }

type GiraffeErrorHandlerMiddleware ( next : RequestDelegate, errorHandler : ErrorHandler, loggerFactory : ILoggerFactory) =
    do 
        if isNull next then raise (ArgumentNullException("next"))    

    member __.Invoke (ctx : HttpContext) =
        task {
            try return! next.Invoke ctx
            with ex ->
                let logger = loggerFactory.CreateLogger<GiraffeErrorHandlerMiddleware>()
                try
                    let func = (Some >> Task.FromResult)
                    let! _ = errorHandler ex logger func ctx
                    return () 
                with ex2 ->
                    logger.LogError(EventId(0), ex , "An unhandled exception has occurred while executing the request.") 
                    logger.LogError(EventId(0), ex2, "An execption was thrown attempting to handle the original exception")
        }

type IApplicationBuilder with 
    member this.UseGiraffe (handler : HttpHandler) =
        this.UseMiddleware<GiraffeMiddleware> handler 
        |> ignore
        
    member this.UseGiraffeErrorHandler (handler : ErrorHandler) =
        this.UseMiddleware<GiraffeErrorHandlerMiddleware>(handler)

type IServiceCollection with 
    member this.AddGiraffe() =
        this.TryAddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer(NewtonsoftJsonSerializer.DefaultSettings))
        this.TryAddSingleton<IXmlSerializer>(DefaultXmlSerializer(DefaultXmlSerializer.DefaultSettings))

    