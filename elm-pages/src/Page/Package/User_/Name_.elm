module Page.Package.User_.Name_ exposing (Data, Model, Msg, page)

import Data
import DataSource exposing (DataSource)
import DataSource.Http
import Head
import Head.Seo as Seo
import Html
import Markdown
import OptimizedDecoder as Decode
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Secrets as Secrets
import Pages.Url
import Result
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    { user : String, name : String }


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , routes = routes
        , data = data
        }
        |> Page.buildNoState { view = view }


packagesDataSource : DataSource (List Data.PackageInfo)
packagesDataSource =
    DataSource.Http.get
        (Secrets.succeed "https://package.elm-lang.org/search.json")
        (Decode.list Data.packageInfoDecoder)


routes : DataSource (List RouteParams)
routes =
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


data : RouteParams -> DataSource Data
data routeParams =
    DataSource.Http.unoptimizedRequest
        (Secrets.succeed
            { url = "https://package.elm-lang.org/packages/" ++ routeParams.user ++ "/" ++ routeParams.name ++ "/latest/README.md"
            , method = "GET"
            , headers = []
            , body = DataSource.Http.emptyBody
            }
        )
        (DataSource.Http.expectString Result.Ok)


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
    String


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    let
        toMarkdown =
            static.data
                |> Markdown.toHtml []
    in
    { title = ""
    , body = [ toMarkdown ]
    }
