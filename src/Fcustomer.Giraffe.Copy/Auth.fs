module FCustomer.Giraffe.Copy.Auth

open System
open FSharp.Control.Tasks.V2.ContextInsensitive
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authorization
open System.Security.Claims
open FCustomer.Giraffe.Copy.Common

let challenge (authSchema : string) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            do! ctx.ChallengeAsync authSchema
            return! next ctx
        }

let signOut (authSchema : string) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            do! ctx.SignOutAsync authSchema
            return! next ctx
        } 

let authorizeRequest (predicate : HttpContext -> bool) (authFailedHandler : HttpHandler) : HttpHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        (if predicate ctx then next else authFailedHandler earlyReturn) ctx

let private evaluateUserPolicy (policy : ClaimsPrincipal -> bool) (authFailedHandler : HttpHandler) : HttpHandler =
    authorizeRequest (fun ctx -> policy ctx.User) authFailedHandler

let authorizeUser = evaluateUserPolicy

let requiresAuthentication (authFailedHandler : HttpHandler) : HttpHandler =
    authorizeUser (fun user -> isNotNull user && user.Identity.IsAuthenticated) authFailedHandler

let requiresRole (role : string) (authFailedHandler : HttpHandler) : HttpHandler =
    authorizeUser (fun user -> user.IsInRole role) authFailedHandler

let requiresRoleOf (roles : string list) (authFailedHandler : HttpHandler) : HttpHandler =
    authorizeUser (fun user -> List.exists user.IsInRole roles) authFailedHandler 

// let authorizeByPolicyName (policyName : string) (authFailedHandler : HttpHandler) : HttpHandler =
//     fun (next : HttpFunc) (ctx : HttpContext) ->
//         task {
//             let authService = ctx.GetService<IAuthenticationService>()
//             let! result = authService.AuthorizeAsync(ctx.User, policyName)
//         }