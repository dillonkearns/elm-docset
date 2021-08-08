module Page.Package.User_.Name_.SPLAT_ exposing (Data, Model, Msg, page)

import Data
import DataSource exposing (DataSource)
import DataSource.Http
import Head
import Head.Seo as Seo
import Html
import Html.Attributes as Attrs
import Markdown
import OptimizedDecoder as Decode
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Secrets as Secrets
import Pages.Url
import Path
import Result
import Route
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    { user : String, name : String, splat : ( String, List String ) }


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , routes = routes
        , data = data
        }
        |> Page.buildNoState { view = view }


packagesDataSource =
    DataSource.Http.get
        (Secrets.succeed "https://package.elm-lang.org/search.json")
        (Decode.list Data.packageInfoDecoder)


modulesDataSource : String -> String -> DataSource {user: String, name: String, modules: (List Data.Module)}
modulesDataSource user name =
    DataSource.Http.get
        (Secrets.succeed ("https://package.elm-lang.org/packages/" ++ user ++ "/"++ name ++ "/latest/docs.json"))
        (Decode.list Data.moduleDecoder)
        |> DataSource.map
                                    (\modules ->
                                        { user = user, name = name, modules = modules }
                                    )


testingDataSource =
    DataSource.succeed [ { name = "elm/browser", version = "123", summary = "smth" } ]


routes : DataSource (List RouteParams)
routes =
    let
        toSplats : String -> String -> List Data.Module -> List RouteParams
        toSplats user name modules =
            List.map (\module_ -> { user = user, name = name, splat = ( module_.slug, [] ) }) modules

        toRouteParams =
            List.map
                (\{user, name, modules} -> toSplats user name modules)
                >> List.concat
    in
    packagesDataSource
        --|> DataSource.map (List.take 10)
        |> DataSource.andThen
            (\packages ->
                packages
                    |> List.map
                        (\{splittedName} ->
                            modulesDataSource (Tuple.first splittedName) (Tuple.second splittedName)
                        )
                    |> DataSource.combine
            )
        |> DataSource.map toRouteParams


data : RouteParams -> DataSource Data
data routeParams =
    modulesDataSource routeParams.user routeParams.name
        |> DataSource.andThen
            (\{modules} ->
              modules
              |> List.filter (\{ slug } -> Tuple.first routeParams.splat == slug)
                    |> List.head
                    |> Maybe.map (\moduleData -> DataSource.succeed { user = routeParams.user, name = routeParams.name, moduleData = moduleData })
                    |> Maybe.withDefault (DataSource.fail "")
            )


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


type alias Data =
    { user : String
    , name : String
    , moduleData : Data.Module
    }


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    let
        toMarkdown =
            static.data.moduleData.comment
                |> Markdown.toHtml []

        toLink user name =
            Html.a
                [ Route.Package__User___Name_ { user = user, name = name }
                    |> Route.toPath
                    |> Path.toAbsolute
                    |> Attrs.href
                ]
                [ Html.text <| user ++ "/" ++ name ]
    in
    { title = ""
    , body =
       [ toLink static.data.user static.data.name
       , Html.h1 [] [ Html.text static.data.moduleData.name ]
       , Html.hr [] []
       , toMarkdown
       ]
    }
