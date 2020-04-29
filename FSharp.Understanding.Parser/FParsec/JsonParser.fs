module FSharp.Understanding.Parser.FParsec.JsonParser

open FParsec
open FSharp.Understanding.Parser.FParsec.JsonAst
open Xunit

let jnull = stringReturn "null" JNull
let jtrue = stringReturn "true" (JBool true)
let jfalse = stringReturn "false" (JBool false)
let jnumber = pfloat |>> JNumber
let str s = pstring s

let stringLiteral =
    let escape = anyOf "\"\\/bfnrt"
                |>> function
                    | 'b' -> "\b"
                    | 'f' -> "\f"
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
                    | c -> string c
                    
    let unicodeEscape =
        let hex2int c = (int c &&& 15 ) + (int c >>> 6)*9
        str "u" >>.pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
            )
        
    let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet = many1Satisfy (fun c -> c <> '"' && c > '\\')
    between (str "\"") (str "\"")
        (stringsSepBy normalCharSnippet escapedCharSnippet)
        
let jstring = stringLiteral |>> JString
let jvalue, jvalueRef = createParserForwardedToRef()

let ws = spaces
let listBetweenStrings sOpen sClose pElement f =
    between (str sOpen) (str sClose)
        (ws >>. sepBy (pElement .>> ws) (str "," .>> ws) |>> f)
        
        
let keyValue = tuple2 stringLiteral (ws >>. str ":" >>. ws >>. jvalue)
let jlist = listBetweenStrings "[" "]" jvalue JList

let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)

do jvalueRef := choice [jobject
                        jlist
                        jstring
                        jnumber
                        jtrue
                        jfalse
                        jnull]

let json = ws >>. jvalue .>> ws .>> eof

let parseJsonString str = run json str

// UTF8 is the default, but it will detect UTF16 or UTF32 byte-order marks automatically
let parseJsonFile fileName encoding =
    runParserOnFile json () fileName encoding

let parseJsonStream stream encoding =
    runParserOnStream json () "" stream System.Text.Encoding.UTF8


module test =
    
    [<Fact>]
    let ``read json from file`` () =
        let fileName = "1.json"
        let result = parseJsonFile fileName System.Text.Encoding.Default
        
        ()


