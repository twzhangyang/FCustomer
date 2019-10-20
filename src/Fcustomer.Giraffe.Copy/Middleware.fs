
module FCustomer.Giraffe.Copy.Middleware
open Microsoft.AspNetCore.Http
open FCustomer.Giraffe.Copy.Core
open System
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open FSharp.Control.Tasks.V2.ContextInsensitive

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