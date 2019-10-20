module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () =
    Assert.True(true)


[<Fact>]
let ``My second Test`` () =
    let a = 1
    let sum = a + 2
    let compare = sum = 3
    Assert.True(compare)
