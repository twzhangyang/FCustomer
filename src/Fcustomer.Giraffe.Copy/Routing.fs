[<AutoOpen>]
module FCustomer.Giraffe.Copy.Routing
open Microsoft.AspNetCore.Http
open FCustomer.Giraffe.Copy.Common
open FSharp.Control.Tasks.V2.ContextInsensitive
open System.Collections.Generic
open System
open System.Text.RegularExpressions
open FCustomer.Giraffe.Copy.FormatExpressions
open Microsoft.Extensions.Primitives

module SubRouting =

    [<Literal>]
    let private RouteKey = "giraffe_route"

    let getSavedPartialPath (ctx : HttpContext) =
        if ctx.Items.ContainsKey RouteKey
        then ctx.Items.Item RouteKey |> string |> strOption
        else None

    let getNextPartOfPath (ctx : HttpContext) =
        match getSavedPartialPath ctx with 
        | Some p when ctx.Request.Path.Value.Contains p -> ctx.Request.Path.Value.[p.Length..]
        | _ -> ctx.Request.Path.Value
        

    let routeWithPartialPath (path : string) (handler : HttpHandler) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            task {
                let savedPartialPath = getSavedPartialPath ctx 
                ctx.Items.Item RouteKey <- ((savedPartialPath |> Option.defaultValue "") + path)
                let! result = handler next ctx
                match result with 
                | Some _ -> ()
                | None -> 
                   match savedPartialPath with 
                   | Some subPath -> ctx.Items.Item RouteKey <- subPath
                   | None -> ctx.Items.Remove RouteKey |> ignore
                return result
            }

    let routePorts (fns : (int * HttpHandler) list) : HttpHandler =
        fun next ->
            let portMap = Dictionary<_, _>(fns.Length)
            fns |> List.iter ( fun (p, h) -> portMap.Add(p, h next ))
            fun (ctx : HttpContext) -> 
                let port = ctx.Request.Host.Port
                if port.HasValue then 
                    match portMap.TryGetValue port.Value with 
                    | true, func -> func ctx
                    | false, _ -> skipPipeline
                else skipPipeline

    let route (path : string) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            if (getNextPartOfPath ctx).Equals path 
            then next ctx
            else skipPipeline

    let routeCi (path : string) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            if String.Equals(getNextPartOfPath ctx, path, StringComparison.CurrentCultureIgnoreCase)
            then next ctx
            else skipPipeline

    let routex (path : string) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            let pattern = sprintf "^%s$" path
            let regex = Regex(pattern, RegexOptions.Compiled)
            let result = regex.Match (getNextPartOfPath ctx)
            match result.Success with 
            | true -> next ctx
            | false -> skipPipeline

    let routeCix (path : string) : HttpHandler =
        fun (next: HttpFunc) (ctx : HttpContext) ->
            let pattern = sprintf "^%s$" path 
            let regex = Regex(pattern, RegexOptions.IgnoreCase ||| RegexOptions.Compiled)
            let result = regex.Match (getNextPartOfPath ctx)
            match result.Success with 
            | true -> next ctx
            | false -> skipPipeline

    let routef (path : PrintfFormat<_,_,_,_, 'T>) (routeHandler  : 'T -> HttpHandler) : HttpHandler =
        validateFormat path
        fun (next : HttpFunc) (ctx : HttpContext) ->
            tryMatchInput path (getNextPartOfPath ctx) false 
            |> function 
            | None -> skipPipeline
            | Some args -> routeHandler args next ctx

    let routeCif (path : PrintfFormat<_,_,_,_, 'T>) (routeHandler : 'T -> HttpHandler) : HttpHandler =
        validateFormat path
        fun (next : HttpFunc) (ctx : HttpContext) ->
            tryMatchInput path (getNextPartOfPath ctx) true
            |> function
            | None      -> skipPipeline
            | Some args -> routeHandler args next ctx

    let routeBind<'T> (route : string) (routeHandler : 'T -> HttpHandler) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            let pattern = route.Replace("{", "(?<").Replace("}",">[^/\n]+)") |> sprintf "^%s$"
            let regex = Regex(pattern, RegexOptions.IgnoreCase)
            let result = regex.Match(getNextPartOfPath ctx)
            match result.Success with 
            | true ->
                let groups = result.Groups
                let result =
                    regex.GetGroupNames()
                    |> Array.skip 1
                    |> Array.map (fun n -> n, StringValues groups.[n].Value)
                    |> dict
                    |> ModelParser.tryParse None
                match result with 
                | Error _ -> skipPipeline
                | Ok model -> routeHandler model next ctx
            | _ -> skipPipeline

    let routeStartsWith (subPath : string) : HttpHandler =
        fun (next : HttpFunc) (ctx : HttpContext) ->
            if (getNextPartOfPath ctx).StartsWith subPath
            then next ctx
            else skipPipeline
            



