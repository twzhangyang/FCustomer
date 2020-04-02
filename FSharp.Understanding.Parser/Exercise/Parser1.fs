module FSharp.Understanding.Parser.Exercise.Parser1
open System
open Xunit

let A_Parser str =
    if String.IsNullOrEmpty(str) then
        (false, "")
    else if str.[0] = 'A' then
        let remaining = str.[1..]
        (true, remaining)
    else
        (false, str)
        
let pchar (charToMatch, str) =
    if String.IsNullOrEmpty(str) then
        let msg = "No more input"
        (msg, "")
    else
        let first = str.[0]
        if first = charToMatch then
            let remainig = str.[1..]
            let msg = sprintf "Found %c" charToMatch
            (msg, remainig)
        else
            let msg = sprintf "Expecting '%c', Got '%c'" charToMatch first
            (msg, str)
            
type Result<'a> =
    | Success of 'a
    | Failure of string
    
let pchar' (charToMatch, str) =
    if String.IsNullOrEmpty str then
        Failure "No more input"
    else
        let firstChar = str.[0]
        if firstChar = charToMatch then
            Success (charToMatch, str.[1..])
        else
            let msg = sprintf "Expecting '%c', Got '%c'" charToMatch firstChar
            Failure msg
            
let pchar'' charToMatch str =
    if String.IsNullOrEmpty str then
        Failure "No more input"
    else
        let first = str.[0]
        if first = charToMatch then
            Success(first, str.[1..])
        else
            let msg = sprintf "Expecting '%c', Got '%c'" charToMatch first
            Failure msg
            
let pchar''' charToMatch =
    let innerFn str =
        if String.IsNullOrEmpty str then
            Failure "No more input"
        else
            let first = str.[0]
            if first = charToMatch then
                Success(first, str.[1..])
            else
                let msg = sprintf "Expecting '%c', Got '%c'" charToMatch first
                Failure msg
    innerFn
            
type Parser<'T> = Parser of (string -> Result<'T * string>)

let pCharFinal charToMatch =
    let innerFn str =
        if String.IsNullOrEmpty str then
            Failure("No more input")
        else
            let first = str.[0]
            if first = charToMatch then
                Success(first, str.[1..])
            else
                let msg = sprintf "Expecting '%c', Got '%c'" charToMatch first
                Failure msg
    Parser innerFn

let run parser input =
    let (Parser innerFn) = parser
    innerFn input
    
let parseA = pCharFinal 'A'
let parseB = pCharFinal 'B'
//let parseAThenB = parseA >> parseB
            
let andThen parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        | Failure msg -> Failure msg
        | Success(first, remaning1) ->
            let result2 = run parser2 remaning1 
            match result2 with
            | Failure msg -> Failure msg
            | Success(first2, remaining2) ->
                let newValue = (first, first2)
                Success(newValue, remaining2)
    Parser innerFn
    
let ( .>>. ) = andThen

let orElse parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        | Success(first1, remaning1) ->
            Success(first1, remaning1)
        | Failure msg1 ->
            let result2 = run parser2 input
            result2
    Parser innerFn

let ( <|> ) = orElse

let choice listOfParsers =
    List.reduce ( <|> ) listOfParsers
    
let anyOf listOfChars =
    listOfChars
    |> List.map pCharFinal
    |> choice
    
let parseLowercase =
    anyOf ['a'..'z']

let parseDigit =
    anyOf ['0'..'9']
    
let parseThreeDigits =
    parseDigit .>>. parseDigit .>>. parseDigit
    
let mapP f parser =
    let interFn input =
        let result = run parser input
        match result with
        | Success (value, remainig) ->
            let newValue = f value 
            Success(newValue, remainig)
        | Failure err ->
            Failure err
    Parser interFn
    
let ( <!> ) = mapP
let ( |>> ) x f = mapP f x

let parseThreeDigitsAsStr =
    let transformTuple ((c1, c2), c3) =
        String [| c1; c2; c3 |]
        
    mapP transformTuple parseThreeDigits

let parseThreeDigitsAsInt =
    mapP int parseThreeDigitsAsStr
    
let returnP x =
    let innerFn input =
        Success(x, input)
    Parser innerFn
    
let applyP fP xP =
    (fP .>>. xP)
    |> mapP (fun (f, x) -> f x)
    
let ( <*> ) = applyP

let lift2 f xP yP =
    returnP f <*> xP <*> yP
    
let lift2' f xP yP =
    f <!> xP <*> yP

let rec sequence parserList =
    let cons head tail = head::tail
    let consP = lift2 cons
    
    match parserList with
    | [] -> returnP  []
    | head::tail ->
        consP head (sequence tail)

let charListToStr charList =
    String(List.toArray charList)
    
let pstring str =
    str
    |> List.ofSeq
    |> List.map pCharFinal
    |> sequence
    |> mapP charListToStr

let rec parseZeroOrMore parser input =
    let firstResult = run parser input
    match firstResult with
    | Failure err ->
        ([], input)
    | Success (first, inputAfterFirstParse) ->
        let (subsequentValues, remainingInput) =
            parseZeroOrMore parser inputAfterFirstParse
        let values = first::subsequentValues
        (values, remainingInput)
        
let many parser =
    let rec innerFn input =
        Success (parseZeroOrMore parser input)
    
    Parser innerFn
    

let many1 parser =
    let rec innerFn input =
        let firstResult = run parser input
        match firstResult with
        | Failure err ->
            Failure err
        | Success (firstValue, inputAfterFirstParse) ->
            let (subsequentValues, remainingInput) =
                parseZeroOrMore parser inputAfterFirstParse
            let values = firstValue::subsequentValues
            Success (values, remainingInput)
    Parser innerFn
    
let pint =
    let digit= anyOf ['0'..'9']
    let digits = many1 digit
    
    digits
    |> mapP (fun chars -> int (charListToStr chars))

let opt p =
    let some = p |>> Some
    let none = returnP None
    
    some <|> none
    
let pint' =
    let resultToInt (sign, charList) =
        let i = String(List.toArray charList) |> int
        match sign with
        | Some ch -> -i
        | None -> i
    
    let digit = anyOf ['0'..'9']
    let digits = many1 digit
    
    opt (pCharFinal '-') .>>. digits
    |>> resultToInt
    
    
let (.>>) p1 p2 =
    p1 .>>. p2
    |> mapP (fun (a, b) -> a)
    
let (>>.) p1 p2 =
    p1 .>>. p2
    |> mapP (fun (a, b) -> b)
    
let between p1 p2 p3 =
    p1 >>. p2 .>> p3
    
let sepBy1 p sep =
    let sepThenP = sep >>. p
    p .>>. many sepThenP
    |>> fun (p, pList) -> p::pList
    
let sepBy p sep =
    sepBy1 p sep <|> returnP []
  
let bindP f p =
    let innerFn input =
        let result1 = run p input
        match result1 with
        | Failure err ->
            Failure err
        | Success (value1, remainingInput) ->
            let p2 = f value1
            run p2 remainingInput
            
    Parser innerFn
    
let ( >>= ) p f = bindP f p

let mapP' f =
    bindP ( f >> returnP )
    
let andThen' p1 p2 =
    p1 >>= (fun p1Result ->
        p2 >>= (fun p2Result ->
            returnP (p1Result, p2Result)
            ))
    
let applyP' fP xP =
    fP >>= (fun f ->
        xP >>= (fun x ->
            returnP (f x)))
    
let many1' p =
   p >>= (fun head ->
       many p >>= (fun tail ->
           returnP (head::tail))) 

module Tests =

    [<Fact>]
    let ``test parser A`` () =
        let inputABC = "ABC"
        let result = A_Parser inputABC 
        
        Assert.Equal((true, "BC"), result)
        
        
    [<Fact>]
    let ``parse ABC`` () =
        let inputABC = "ABC"
        let result = pchar ('A', inputABC)
        
        Assert.Equal(("Found A", "BC"), result)
        
    [<Fact>]
    let ``parse ABC by Result`` () =
        let inputABC = "ABC"
        let result = pchar' ('A', inputABC)
        
        Assert.Equal((Success('A', "BC")), result)


    [<Fact>]
    let ``parse ABC by curried function`` () =
        let parserA = pchar'' 'A'
        let result = parserA "ABC"
        
        Assert.Equal((Success('A', "BC")), result)
        
        
    [<Fact>]
    let ``parse ABC by final parser`` () =
        let parserA = pCharFinal 'A'
        let result = run parserA "ABC" 
        
        Assert.Equal(Success('A', "BC"), result)
        
    let ``test andThen`` () =
        let parserA = pCharFinal 'A'
        let parserB = pCharFinal 'B'
        
        let parserAParserB = parserA .>>. parserB 
        let result = run parserAParserB "ABC"
        
        Assert.Equal(Success(('A', 'B'), "C"), result)
        
        
    [<Fact>]
    let ``test orElse`` () =
        let parserA = pCharFinal 'A'
        let parserB = pCharFinal 'B'
        
        let parserAOrParserB = parserA <|> parserB
        let result = run parserAOrParserB "BCD"
        
        Assert.Equal(Success('B', "CD"), result)
        
    [<Fact>]
    let ``test anyOf`` () =
        let result = run parseLowercase "aBC"
        Assert.Equal(Success('a', "BC"), result)
        
        let result2 = run parseDigit "9ABC"
        Assert.Equal(Success('9', "ABC"), result2)
        
        
    [<Fact>]
    let ``parse three digit as str`` () =
        let result = run parseThreeDigitsAsStr "123B"
        
        Assert.Equal(Success("123", "B"), result)
        
    [<Fact>]
    let ``parse three digit as int`` () =
        let result = run parseThreeDigitsAsInt "123B"
        
        Assert.Equal(Success(123, "B"), result)
        
    [<Fact>]    
    let ``test sequnce`` () =
        let parsers = [ pCharFinal 'A'; pCharFinal 'B'; pCharFinal 'C' ]
        let combined = sequence parsers
        
        let result = run combined "ABCD"
        Assert.Equal(Success(['A'; 'B'; 'C'], "D"), result)
        
    [<Fact>]
    let ``test pstring`` () =
        let parseABC = pstring "ABC"
        let result = run parseABC "ABCDE"
        
        Assert.Equal(Success("ABC", "DE"), result)

    [<Fact>]
    let ``test many`` () =
        let manyA = many (pCharFinal 'A')
        
        let result = run manyA "AABCD"
        Assert.Equal(Success(['A'; 'A'], "BCD"), result)
        
    
    [<Fact>]
    let ``get any of whitespace char`` () =
        let whitespaceChar = anyOf [' '; '\t'; '\n']
        let whitespae = many whitespaceChar
        
        let result = run whitespae "\tABC"
        
        Assert.Equal(Success(['\t'], "ABC"), result)
        
    [<Fact>]
    let ``test many1`` () =
        let digit = anyOf ['0'..'9']
        let digits = many1 digit
        
        let result = run digits "123BC"
        
        Assert.Equal(Success(['1'; '2'; '3'], "BC"), result)
        
    [<Fact>]
    let ``test pint`` () =
        let result = run pint "123BC"
        
        Assert.Equal(Success(123, "BC"), result)
        
    [<Fact>]
    let ``opt test`` () =
        let digit = anyOf ['0'..'9']
        let digitThenSemicolon = digit .>>. opt (pCharFinal ':')
        
        let result = run digitThenSemicolon "1:"
        
        Assert.Equal(Success(('1', Some ':'), ""), result)
        
        
    [<Fact>]
    let ``- pint`` () =
        let result = run pint' "-123BC"
        
        Assert.Equal(Success(-123, "BC"), result)
        
    [<Fact>]
    let ``throwing results away`` () =
        let digit = anyOf ['0'..'9']
        
        let digitThenSemicolon = digit .>> opt (pCharFinal ':')
 
        let result = run digitThenSemicolon "1:"
        
        Assert.Equal(Success('1', ""), result)
        
    [<Fact>]
    let ``test AB followed by whitespace chars`` () =
        let whitespaceChar = anyOf [ ' '; '\t'; '\n' ]
        let whitespace = many1 whitespaceChar
        
        let ab = pstring "AB"
        let cd = pstring "CD"
        let ab_cd = (ab .>> whitespace) .>>.cd
        
        let result = run ab_cd "AB \t\nCD"
        
        Assert.Equal(Success(("AB", "CD"), ""), result)
        
        
    [<Fact>]    
    let ``test between`` () =
        let pDoubleQuote = pCharFinal '"'
        let quotedInteger = between pDoubleQuote pint pDoubleQuote
        
        let result = run quotedInteger "\"1234\""
        
        Assert.Equal(Success(1234, ""), result)
       
    [<Fact>]
    let ``one or more digit list`` () =
        let comma = pCharFinal ','
        let digit = anyOf [ '0'..'9' ]
        
        let zeroOrMoreDigitList = sepBy digit comma
        let oneOrMoreDigitList = sepBy1 digit comma
        
        let result0 = run zeroOrMoreDigitList "A"
        let result1 = run oneOrMoreDigitList "1,2,3;"
        
        Assert.Equal(Success([],"A"), result0)
        Assert.Equal(Success(['1';'2';'3'], ";"), result1)
        
        
    [<Fact>]    
    let ``cast int list`` () =
        
        let comma = pCharFinal ','
        let digit = anyOf [ '0'..'9' ]
        let oneOrMoreDigitList = sepBy1 digit comma

        let charListToIntList (charList:char list) =
            charList 
            |> List.map (fun item -> Int32.Parse(string item))
           
        let list = oneOrMoreDigitList |>> charListToIntList 
        let resultList = run list "1,2,3;"
        let expected = Success([1;2;3], ";") 
       
        Assert.Equal(expected, resultList)
