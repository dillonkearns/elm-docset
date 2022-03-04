module Api.Name exposing (route)

import ApiRoute
import Data
import DataSource exposing (DataSource)
import DataSource.Http
import Html exposing (Html)
import Html.Attributes as Attrs
import Json.Decode as Decode
import Markdown
import Result


route : (Html msg -> String) -> ApiRoute.ApiRoute ApiRoute.Response
route htmlToString =
    ApiRoute.succeed
        (\user name ->
            data user name
                |> DataSource.map (\resolvedData -> resolvedData |> view user name |> htmlToString)
        )
        |> ApiRoute.literal "package"
        |> ApiRoute.slash
        |> ApiRoute.capture
        |> ApiRoute.slash
        |> ApiRoute.capture
        |> ApiRoute.slash
        -- temporarily generating this as myindex.html instead of index.html because the elm-pages dev server and build process strip off trailing `/index.html` to match ApiRoutes or Page Modules
        |> ApiRoute.literal "myindex.html"
        |> ApiRoute.preRender pages


packagesDataSource : DataSource (List Data.PackageInfo)
packagesDataSource =
    DataSource.Http.get
        "https://package.elm-lang.org/search.json"
        (Decode.list Data.packageInfoDecoder)


modulesDataSource : String -> String -> DataSource { user : String, name : String, modules : List Data.Module }
modulesDataSource user name =
    DataSource.Http.get
        ("https://package.elm-lang.org/packages/" ++ user ++ "/" ++ name ++ "/latest/docs.json")
        (Decode.list Data.moduleDecoder)
        |> DataSource.map
            (\modules ->
                { user = user, name = name, modules = modules }
            )


readmeDataSource user name =
    DataSource.Http.request
        { url = "https://package.elm-lang.org/packages/" ++ user ++ "/" ++ name ++ "/latest/README.md"
        , method = "GET"
        , headers = []
        , body = DataSource.Http.emptyBody
        }
        DataSource.Http.expectString


pages toRoute =
    let
        toRouteParams =
            List.map
                (\{ splittedName } ->
                    toRoute
                        (Tuple.first splittedName)
                        (Tuple.second splittedName)
                )
    in
    packagesDataSource
        |> DataSource.map toRouteParams


type alias Data =
    { modulesUrls : List String
    , readme : String
    }


data : String -> String -> DataSource Data
data user name =
    DataSource.map2 (\readme { modules } -> { modulesUrls = List.map .slug modules, readme = readme })
        (readmeDataSource user name)
        (modulesDataSource user name)


view :
    String
    -> String
    -> Data
    -> Html msg
view user name data_ =
    let
        removeLtGt =
            String.replace "<" "&lt;" << String.replace ">" "&gt;"

        toMarkdown =
            data_.readme
                |> removeLtGt
                |> Markdown.toHtml []

        toLink user_ name_ moduleName =
            Html.a
                [ Attrs.href <| "./" ++ moduleName
                ]
                [ Html.text <| moduleName ]

        modules =
            data_.modulesUrls
                |> List.map (\url -> Html.li [] [ toLink user name url ])
    in
    Html.div [] [ Html.div [] [ Html.ul [] modules ], toMarkdown ]
