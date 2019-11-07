module Giraffe.SampleApp.HtmlViews

open Giraffe.GiraffeViewEngine
open Giraffe.SampleApp.Model

let layout (content: XmlNode list) =
    html [] [
        head [] [
            title [] [ str "Giraffe" ]
        ]
        body [] content
    ]
    
let partial () =
    p [] [ str "SOme partial text." ]
    
let personView (model : Person) =
    [
        div [ _class "container" ] [
            h3 [_title "Some title attribute"] [sprintf "Hello, %s" model.Name |> str ]
            a [_href "https://github.com" ] [str "github"]
        ]
        div [] [ partial() ]
    ] |> layout


