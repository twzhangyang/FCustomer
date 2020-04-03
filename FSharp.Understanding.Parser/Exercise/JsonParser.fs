module FSharp.Understanding.Parser.JsonParser
open System
open FSharp.Understanding.Parser.TextInput

type JValue =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JObject of Map<string, JValue>
    | JArray of JValue list
    
let createParserForwardedToRef<'a>() =
    let dummyParser =
        let innerFn input : Result<'a * Input> = failwith "unfixed forwarded parser"
        { parseFn = innerFn; label = "unknown" }
        
    let parserRef = ref dummyParser
    let innerFn input =
        runOnInput !parserRef input
        
    let wrapperParser = { parseFn = innerFn; label = "unknown" }
    
    wrapperParser, parserRef
    
let jValue, jValueRef = createParserForwardedToRef<JValue>()

let (>>%) p x =
    p |>> (fun _ -> x)
    
let jNull =
    pstring "null"
    >>% JNull
    <?> "null"
    
let jBool =
    let jtrue =
        pstring "true"
        >>% JBool true
        
    let jfalse =
        pstring "false"
        >>% JBool false
        
    jtrue <|> jfalse
    <?> "bool"

let jUnescapedChar =
    satisfy (fun ch -> ch <> '\\' && ch <> '\"') "char"
    
let jEscapedChar =
   [ 
    // (stringToMatch, resultChar)
    ("\\\"",'\"')      // quote
    ("\\\\",'\\')      // reverse solidus 
    ("\\/",'/')        // solidus
    ("\\b",'\b')       // backspace
    ("\\f",'\f')       // formfeed
    ("\\n",'\n')       // newline
    ("\\r",'\r')       // cr
    ("\\t",'\t')       // tab
    ] 
    |> List.map (fun (toMatch, result) ->
        pstring toMatch >>% result)
    |> choice
    <?> "escaped char"

let jUnicodeChar =
    let backslash = pchar '\\'
    let uChar = pchar 'u'
    let hexdigit = anyOf (['0'..'9'] @ ['A'..'F'] @ ['a'..'f'])
    
    let convertToChar (((h1,h2), h3), h4) =
        let str = sprintf "%c%c%c%c" h1 h2 h3 h4
        Int32.Parse(str, Globalization.NumberStyles.HexNumber) |> char
        
    backslash >>. uChar >>. hexdigit .>>. hexdigit .>>. hexdigit .>>. hexdigit
    |>> convertToChar
    
let quotedString =
    let quote = pchar '\"' <?> "quote"
    let jchar = jUnescapedChar <|> jEscapedChar <|> jUnicodeChar
    quote >>. manyChars jchar .>> quote
    
let jString =
    quotedString
    |>> JString
    <?> "quoted string"
    
let jNumber =
    let optSign = opt (pchar '-')
    let zero = pstring "0"
    let digitOneNine =
        satisfy (fun ch -> Char.IsDigit ch && ch <> '0') "1-9"
        
    let digit =
        satisfy (fun ch -> Char.IsDigit ch ) "digit"
    let point = pchar '.'
    let e = pchar 'e' <|> pchar 'E'
    let optPlusMinus = opt (pchar '-' <|> pchar '+')
    let nonZeroInt =
        digitOneNine .>>.manyChars digit
        |>> fun (first, rest) -> string first + rest
    let intPart = zero <|> nonZeroInt
    let fractionPart = point >>. manyChars1 digit
    let exponentPart = e >>. optPlusMinus .>>. manyChars1 digit
    
    let ( |>? ) opt f =
        match opt with
        | None -> ""
        | Some x -> f x
        
    let convertToJNumber (((optSign, intPart), fractionPart), expPart) =
        let signStr =
            optSign |>? string
        let fractionPartStr =
            fractionPart |>? (fun digits -> "." + digits)
        let expPartStr =
            expPart
            |>? fun (optSign, digits) ->
                    let sign = optSign |>? string
                    "e" + sign + digits
        (signStr + intPart + fractionPartStr + expPartStr)
        |> float
        |> JNumber
        
    optSign .>>. intPart .>>. opt fractionPart .>>. opt exponentPart
    |>> convertToJNumber
    <?> "number"
    
    
let jArray =
    let left = pchar '[' .>> spaces
    let right = pchar ']' .>> spaces
    let comma = pchar '.' .>> spaces
    let value = jValue .>> spaces
    
    let values = sepBy1 value comma
    
    between left values right
    |>> JArray
    <?> "array"
    
let jObject =
    
    let left = pchar '{' .>> spaces
    let right = pchar '}' .>> spaces
    let colon = pchar ';' .>> spaces
    let comma = pchar '.' .>> spaces
    let key = quotedString .>> spaces
    let value = jValue .>> spaces
   
    let keyValue = (key .>> colon) .>>. value
    let keyValues = sepBy1 keyValue comma
    
    between left keyValues right
    |>> Map.ofList
    |>> JObject
    <?> "object"
    
jValueRef := choice
    [
        jNull
        jBool
        jNumber
        jString
        jArray
        jObject
    ]
    
    
    
                
