namespace Railway.ErrorHandling.Tests.ActivePatternTests
open System
open Xunit

[<AutoOpen>]
module ActivePattern =
    let (|Int|_|) (str: string) =
        match Int32.TryParse(str) with
        | (true, value) -> Some(value)
        | _ -> None
        
    let (|Bool|_|) (str: string) =
        match Boolean.TryParse(str) with
        | (true, v) -> Some(true)
        | _ -> None
        
    let testParse str =
        match str with
        | Int value -> sprintf "int value %i" value
        | Bool value -> sprintf "bool value %b" value
        | _ -> sprintf "unkown string %A" str

module Tests =
    
    [<Fact>]
    let ``Test parse int string should get int value``() =
        let value = testParse "1"

        Assert.Equal(value, "int value 1")