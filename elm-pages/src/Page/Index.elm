module Page.Index exposing (Data, Model, Msg, page)

import Data
import DataSource exposing (DataSource)
import DataSource.Http
import Head
import Head.Seo as Seo
import Html
import Html.Attributes as Attrs
import OptimizedDecoder as Decode
import OptimizedDecoder.Pipeline as Decode
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Secrets as Secrets
import Pages.Url
import Path
import Route
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


type alias Data =
    List Data.PackageInfo


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


data : DataSource Data
data =
    DataSource.Http.get (Secrets.succeed "https://package.elm-lang.org/search.json")
        (Decode.list Data.packageInfoDecoder)


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
        toLink packageName =
            packageName
                |> String.split "/"
                |> (\values ->
                        case values of
                            [ user, name ] ->
                                Html.a
                                    [ Route.Package__User___Name_ { user = user, name = name }
                                        |> Route.toPath
                                        |> Path.toAbsolute
                                        |> Attrs.href
                                    ]
                                    [ Html.text packageName ]

                            _ ->
                                Html.text ""
                   )
    in
    { title = "Index"
    , body = List.map (\package -> Html.p [] [ toLink package.name, Html.br [] [], Html.text package.summary ]) static.data
    }
