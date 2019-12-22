module Railway.ErrorHandling.Tests.Distribution
open System

type Probability = float
type Distribution<'a> = {
    Sample: unit -> 'a
    Expectation: ('a -> Probability) -> Probability 
}

let always x = {
    Sample = fun () -> x
    Expectation = fun f -> f x
}
     
let rnd = Random()

let assertValidProbability (p: Probability) =
    if p < 0.0 || p > 1.0 then
        failwith "Invalid probability"
    else
        ()
        
let choose (p : Probability) (dist1: Distribution<'a>) (dist2: Distribution<'a>) : Distribution<'a> =
    assertValidProbability p
    let sample() =
        if rnd.NextDouble() < p then
            dist1.Sample()
        else
            dist2.Sample()
            
    let expectation f =
        (p * dist1.Expectation(f)) +
        ((1.0 - p) * dist2.Expectation(f))
    {Sample = sample; Expectation = expectation}
    
let bind dist k =
    let sample() =
        let newDst = k(dist.Sample())
        newDst.Sample()
    let expectation f =
        dist.Expectation(fun x ->
            let newDist = k x
            newDist.Expectation(f)
            )
    {Sample = sample; Expectation = expectation}
    
type DistributionBuilder() =
    member this.Bind(d, k) = bind d k
    member this.Return(x) = always x
    member this.ReturnFrom(d) = d
    
let dist = DistributionBuilder()

type WeightCase<'a> = 'a * Probability

let weightedCases (weightedCases: WeightCase<_> list) =
    let rec loop pUsed list =
        match list with
        | [] ->
            failwith "should never happen"
        | [(x, _)] ->
            always x
        | (x, p) :: rest ->
            let probX = p / (1.0 - pUsed)
            let distX = (always x)
            let pUsed = pUsed + p
            let distOther = loop pUsed rest 
            choose probX distX distOther
    loop 0.0 weightedCases
   
type CountedCase<'a> = 'a * int

let countedCases (cases: CountedCase<_> list) =
    let totalCount =
        cases
        |> List.sumBy (fun (x, count) -> count)
        |> float
        
    cases
    |> List.map (fun (x, count) ->
        let weight = float count / totalCount
        x, weight
        )
    |> weightedCases
    
let countItems list =
    list
    |> List.groupBy (id)
    |> List.map (fun (x, items) -> (x, List.length items))
    
type Coin = Heads | Tails

let fairCoin = countedCases [Heads, 50; Tails, 50]

fairCoin.Expectation (fun x ->
    match x with
    | Heads -> 2.0
    | Tails -> 0.0
    )

List.init 1000 (fun _ -> fairCoin.Sample())
|> countItems

let biasedCoin = countedCases [Heads, 30; Tails, 70]

biasedCoin.Expectation (fun x ->
    match x with
    | Heads -> 2.0
    | Tails -> 0.0
    )

List.init 1000 (fun _ -> biasedCoin.Sample())
|> countItems

type Outcome = Odd | Even | Zero

let roulette = countedCases [Odd, 18; Even, 18; Zero, 1]

roulette.Expectation (fun x ->
    match x with
    | Odd -> 10.0
    | Even -> 0.0
    | Zero -> 0.0
    )

List.init 3700 (fun _ -> roulette.Sample())
|> countItems






            