module Railway.ErrorHandling.Tests.FoldTests
open Xunit

List.fold (fun acc x -> x :: acc) [] [1; 2; 3; 4; 5]
// val it : int list = [5; 4; 3; 2; 1]

List.foldBack (fun x acc -> x :: acc) [1; 2; 3; 4; 5] [];;
// val it : int list = [1; 2; 3; 4; 5]

let firstSumBiggerThan100 listOfInt =
    let fold accumulator number =
        if accumulator > 100 then accumulator
        else accumulator + number
    let initialAcc = 0
    listOfInt |> List.fold fold initialAcc
    

[<Fact>]
let ``should get 120`` () =
    let a = [30; 40; 50 ] |> firstSumBiggerThan100
    
    Assert.Equal(120, a)
    
[<Fact>]
let ``should get 120 too`` () =
    let a = [30; 40; 50; 30] |> firstSumBiggerThan100
    let sumListBack list = List.foldBack (fun elem acc -> acc + elem) list 0
    
    
    Assert.Equal(120, a)
    
    
module Container =
    type Book = {title: string; price: decimal}

    type ChocolateType = Dark | Milk | SeventyPercent
    type Chocolate = {chocType: ChocolateType ; price: decimal}

    type WrappingPaperStyle = 
    | HappyBirthday
    | HappyHolidays
    | SolidColor

    type Gift =
    | Book of Book
    | Chocolate of Chocolate 
    | Wrapped of Gift * WrappingPaperStyle
    | Boxed of Gift 
    | WithACard of Gift * message:string
    
    type Container<'ContentData,'DecorationData> =
    | Contents of 'ContentData
    | Decoration of 'DecorationData * Container<'ContentData,'DecorationData>
    
    type GiftContents = 
    | Book of Book
    | Chocolate of Chocolate 

    // unified data for recursive cases
    type GiftDecoration = 
    | Wrapped of WrappingPaperStyle
    | Boxed 
    | WithACard of string
//
//    type Gift' =
//    // non-recursive case
//    | Contents of GiftContents
//    // recursive case
//    | Decoration of Gift' * GiftDecoration
    
    let rec cata fContents fDecoration (container:Container<'ContentData,'DecorationData>) :'r = 
        let recurse = cata fContents fDecoration 
        match container with
        | Contents contentData -> 
            fContents contentData 
        | Decoration (decorationData,subContainer) -> 
            fDecoration decorationData (recurse subContainer)
            
//     let rec fold fContents fDecoration acc (container:Container<'ContentData,'DecorationData>) :'r = 
//        let recurse = fold fContents fDecoration 
//        match container with
//        | Contents contentData -> 
//            fContents acc contentData 
//        | Decoration (decorationData,subContainer) -> 
//            let newAcc = fDecoration acc decorationData
//            recurse newAcc subContainer