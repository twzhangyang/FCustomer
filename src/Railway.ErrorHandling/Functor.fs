module Railway.ErrorHandling.Functor
open System
open System.Security.Cryptography.X509Certificates

let mapOption f opt =
    match opt with
    | Some x -> Some (f x)
    | None -> None
    
let rec mapList (f: ('a -> 'b)) (list: 'a list) =
    match list with
    | [] -> []
    | head :: tail -> (f head) :: mapList f tail

let a = Some 2 |> mapOption (fun x -> x * 2)
let a'' = [1;2;3] |> mapList (fun x -> x * 2)

let applyOpt fOpt xOpt =
    match fOpt, xOpt with
    | Some f, Some x -> Some ( f x)
    | _, _ -> None
    
let applyList (fList : ('a -> 'b) list) (xList : 'a list) =
    [ for f in fList do 
      for x in xList do
       yield f x
       ]

let (<*>) = applyOpt
let (<!>) = mapOption

let a''' =Some (fun x -> x * 2) <*> Some 2

let add x y = x + y
let a'''' = add <!> (Some 2) <*> (Some 3)

let lift2 f x y =
    f <!> x <*> y
    
let lift3 f x y z =
    f <!> x <*> y <*> z
    
let bindOpt f opt =
    match opt with
    | Some x -> f x
    | None -> None

let bindList (fList : ('a -> 'b) list) (list : 'a list)=
    [for f in fList do
     for a in list do
         yield f a
         ]

let (>>=) = bindOpt

let parseInt str =
    match Int32.TryParse str with
    | true, v -> Some v
    | _, _ -> None
    
let calculatePrice qty =
    if qty < 10
    then Some 20
    else
        Some 3
        
let price =
    parseInt "32"
    |> bindOpt calculatePrice
    
let price'' = calculatePrice >>= (parseInt "32")

        
        
    