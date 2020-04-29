module FSharp.Understanding.Parser.FParsec.Parser1

open FParsec
open Xunit

let test p str =
    match run p str with
    | Success(result, _, _) -> sprintf "Success: %A" result
    | Failure(errorMsg, _, _) -> sprintf "Failure %s" errorMsg
    

[<Fact>]
let ``test parse float`` () =
    let result1 = run pfloat "1.25"
    let result2 = run pfloat "1.25E 2"
    ()
    
[<Fact>]
let ``test parse string``() =
    let str s = pstring s
    let floatBetweenBrackets = str "[" >>.pfloat .>>str "]"
    
    let result = run floatBetweenBrackets "[1.0]"
    let result2 = run floatBetweenBrackets "[]"
    ()

let betweenStrings s1 s2 p = pstring s1 >>. p .>> pstring s2
let floatBetweenBrackets = pfloat |> betweenStrings "[" "]"
let floatBetweenDoubleBrackets = pfloat |> betweenStrings "[[" "]]"
    
[<Fact>]
let ``abstracting parsers``() =
    let result1 = run floatBetweenBrackets "[1.0]"
    let result2 = run floatBetweenDoubleBrackets "[[1.0]]"
    ()
    
[<Fact>]
let ``many float`` () =
    let result1 = run (many floatBetweenBrackets) "[1.0]"
    let result2 = run (many floatBetweenDoubleBrackets) ""
    ()
    
[<Fact>]
let ``many1 float`` () =
    let result1 = run (many1 floatBetweenBrackets) "[1.0]"
    let result2 = run (many1 floatBetweenBrackets) ""
    
    ()
    
let floatList = pstring "[" >>. sepBy pfloat (pstring ",") .>> pstring "]"
    
[<Fact>]
let ``float list`` () =
    let result1 = run floatList "[]"
    let result2 = run floatList "[1.0,2.0]"
    
    ()
    
let ws = spaces
let str_ws s = pstring s .>> ws
let float_ws = pfloat .>> ws
let numberList = str_ws "[" >>.sepBy float_ws (str_ws ",") .>> str_ws "]"

[<Fact>]
let ``float list with whitespace`` () =
    let result = run numberList @"[1 , 2 ]"
    let result1 = run numberList @"[1, 2; 4]"
    
    let numberListFile = ws >>. numberList .>> eof
    let result2 = run numberListFile "[1, 2 ,3] [4]";
    
    ()

// parsing string data
[<Fact>]
let ``parsing string data`` () =
    let result1 = run (many (pstring "a" <|> pstring "b")) "abba"
    let result2 = run (skipStringCI "<float>" >>. pfloat) "<FLOAT>1.0"
    
    ()
    
let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" .>> ws
    
[<Fact>]
let ``test identifier`` () =
    let result = run identifier "_"
    let result2 = run identifier "_test1"
    let result3 = run identifier "1"
    
    ()
   
   
// let stringLiteral =
//     let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
//     let unescape c = match c with
//        | 'n' -> '\n'
//        | 'r' -> '\r'
//        | 't' -> '\t'
//        | c -> c
//        
//    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
//    between (pstring "\"") (pstring "\"")
//    (manyChars (normalChar <|> escapedChar))

// Sequentially applying parsers
let product = pipe2 float_ws (str_ws "*" >>.float_ws) (fun x y -> x * y)

[<Fact>]
let ``test pip2`` () =
    let result1 = run product "3 * 5"
    
    ()
    
type StringConstant = StringConstant of string * string

let stringLiteral = pstring 
//let stringConstant = pipe3 identifier (str_ws "=") stringLiteral (func id a str -> StringConstant(id, str))
    
[<Fact>]
let ``pipe 3 test`` () =
    let result = test (float_ws .>>. (str_ws "," >>.float_ws)) "123, 456"
    
    ()
    
let boolean = (stringReturn "true" true) <|> (stringReturn "false" false)

[<Fact>]
let ``boolean test`` () =
    let result = run boolean "true"
    
    ()
    


    

    
    

