module View exposing (View, map, placeholder)

import Data
import Html exposing (Html)


type alias View msg =
    { title : String
    , body : List (Html msg)
    }


map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title
    , body = List.map (Html.map fn) doc.body
    }


placeholder : String -> View msg
placeholder data =
    { title = "Placeholder"
    , body = [ Html.text data ]
    }
