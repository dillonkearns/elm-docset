module Page.Package.User_.Name_ exposing (Data, Model, Msg, page)

import Data
import DataSource exposing (DataSource)
import DataSource.Http
import Head
import Head.Seo as Seo
import Html
import Html.Attributes as Attrs
import Json.Decode as Decode
import Markdown
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path
import Result
import Route
import Shared
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    { user : String, name : String }


page : Page RouteParams Data
page =
    Page.preRender
        { head = head
        , pages = pages
        , data = data
        }
        |> Page.buildNoState { view = view }


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
        (DataSource.Http.expectString Result.Ok)


pages : DataSource (List RouteParams)
pages =
    let
        toRouteParams =
            List.map
                (\{ splittedName } ->
                    { name = Tuple.second splittedName
                    , user = Tuple.first splittedName
                    }
                )
    in
    packagesDataSource
        |> DataSource.map toRouteParams


type alias Data =
    { modulesUrls : List String
    , readme : String
    }


data : RouteParams -> DataSource Data
data routeParams =
    DataSource.map2 (\readme { modules } -> { modulesUrls = List.map .slug modules, readme = readme })
        (readmeDataSource routeParams.user routeParams.name)
        (modulesDataSource routeParams.user routeParams.name)


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    []


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    let
        removeLtGt =
            String.replace "<" "&lt;" << String.replace ">" "&gt;"

        toMarkdown =
            static.data.readme
                |> removeLtGt
                |> Markdown.toHtml []

        toLink user name moduleName =
            Html.a
                [ Attrs.href <| "./" ++ moduleName
                ]
                [ Html.text <| moduleName ]

        modules =
            static.data.modulesUrls
                |> List.map (\url -> Html.li [] [ toLink static.routeParams.user static.routeParams.name url ])
    in
    { title = ""
    , body = [ Html.div [] [ Html.ul [] modules ], toMarkdown ]
    }
