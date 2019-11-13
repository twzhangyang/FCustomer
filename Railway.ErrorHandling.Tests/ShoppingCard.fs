module Railway.ErrorHandling.Tests.ShoppingCard
open Xunit

type CartItem = string

type EmptyState = NoItems

type ActiveState = {
    UnpaidItems: CartItem list
 }

type PaidState = {
    PaidItems: CartItem list
    PaymentAmount: decimal
 }

type CartState =
    | Empty of EmptyState 
    | Active of ActiveState
    | Paid of PaidState
    
let addToEmptyState item =
    CartState.Active { UnpaidItems = [ item ] }

let addToActiveState state item =
    let items = item :: state.UnpaidItems
    CartState.Active { state with UnpaidItems = items }

let removeFromActiveState state item =
    let items = state.UnpaidItems |> List.filter (fun i -> i <> item)

    match items with
        | [] -> CartState.Empty NoItems
        | _ -> CartState.Active { state with UnpaidItems = items }

let payForActiveState state amount =
    CartState.Paid { PaidItems = state.UnpaidItems;  PaymentAmount = amount}

type EmptyState with
    member __.Add = addToEmptyState
 
type ActiveState with
    member __.Add = addToActiveState __
    member __.Pay = payForActiveState __
    member __.Remove = removeFromActiveState __
    
let addItemToCard cart item =
    match cart with
        | Empty state -> state.Add item
        | Active state -> state.Add item
        | Paid state -> printfn "Can not add item in paid card"
                        cart
                        
let removeItemFromCard cart item =
    match cart with
    | Empty state -> printfn "Can not remove item in empty cart"
                     cart
    | Active state -> state.Remove item
    | Paid state -> printfn "Can not remove item in paid cart"
                    cart
                    
let pay cart amount=
    match cart with
    | Empty state -> printfn "Can not pay in empty cart"
                     cart 
    | Active state -> state.Pay amount
    | Paid state -> printfn "Can not pay in paid cart"
                    cart 
                    
let displayCard cart =
    match cart with
    | Empty state -> sprintf "Empty card"
    | Active state -> sprintf "Active card with items %A" state.UnpaidItems
    | Paid state -> sprintf  "Paid card with items %A, amount is %f" state.PaidItems state.PaymentAmount
    
type CartState with
    static member EmptyCart = Empty NoItems
    member __.Add = addItemToCard __
    member __.Remove = removeItemFromCard __
    member __.Display = displayCard __
    member __.Pay = pay __ 
    
module test =
    [<Fact>]
    let ``empty cart has no items`` () =
        let cart = CartState.EmptyCart
        let newCart = cart.Remove "item"
        
        Assert.Equal(cart, newCart)
    
    [<Fact>]
    let ``Add item in empty cart become active cart`` ()=
        let cart = CartState.EmptyCart
        let newCart = cart.Add "item 1"
        
        Assert.Equal(newCart.Display, "Active card with items [\"item 1\"]")
        
    
    [<Fact>]
    let ``Only can pay for active cart`` () =
        let cart = CartState.EmptyCart
        let activeCart = cart.Add "item 1"
        let paidCart = activeCart.Pay 10.0m
        
        Assert.Equal(paidCart.Display, "Paid card with items [\"item 1\"], amount is 10.000000")        
        
        
        
        
        
        
        