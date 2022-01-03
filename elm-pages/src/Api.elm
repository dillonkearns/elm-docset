module Api exposing (routes)

import ApiRoute
import DataSource exposing (DataSource)
import Html exposing (Html)
import Json.Encode
import Route exposing (Route)


routes :
    DataSource (List Route)
    -> (Html Never -> String)
    -> List (ApiRoute.ApiRoute ApiRoute.Response)
routes getStaticRoutes htmlToString =
    []



{- [ ApiRoute.succeed
       (\author package ->
           DataSource.succeed
               { body = htmlToString (myRenderHtmlFunction author package) }
       )
       |> ApiRoute.literal "api"
       |> ApiRoute.slash
       |> ApiRoute.capture
       |> ApiRoute.slash
       |> ApiRoute.capture
       |> ApiRoute.slash
       |> ApiRoute.literal "latest.html"
       |> ApiRoute.buildTimeRoutes
           (\route ->
               DataSource.succeed
                   [ route "dillonkearns" "elm-pages"
                   , route "elm" "core"
                   ]
           )
   , ApiRoute.succeed
       (\userId ->
           DataSource.succeed
               { body =
                   Json.Encode.object
                       [ ( "id", Json.Encode.int userId )
                       , ( "name"
                         , Html.p [] [ Html.text <| "Data for user " ++ String.fromInt userId ]
                               |> htmlToString
                               |> Json.Encode.string
                         )
                       ]
                       |> Json.Encode.encode 2
               }
       )
       |> ApiRoute.literal "users"
       |> ApiRoute.slash
       |> ApiRoute.int
       |> ApiRoute.literal ".json"
       |> ApiRoute.buildTimeRoutes
           (\route ->
               DataSource.succeed
                   [ route 1
                   , route 2
                   , route 3
                   ]
           )
   ]
-}


myRenderHtmlFunction author package =
    Html.div [] [ Html.text author, Html.text package ]
