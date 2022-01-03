module Api exposing (routes)

import ApiRoute
import Data
import DataSource exposing (DataSource)
import DataSource.Http
import Html exposing (Html)
import Json.Encode
import OptimizedDecoder as Decode
import Pages.Secrets as Secrets
import Route exposing (Route)


modulesDataSource : String -> String -> DataSource { user : String, name : String, modules : List Data.Module }
modulesDataSource user name =
    DataSource.Http.get
        (Secrets.succeed ("https://package.elm-lang.org/packages/" ++ user ++ "/" ++ name ++ "/latest/docs.json"))
        (Decode.list Data.moduleDecoder)
        |> DataSource.map
            (\modules ->
                { user = user, name = name, modules = modules }
            )


packagesDataSource : DataSource (List Data.PackageInfo)
packagesDataSource =
    DataSource.Http.get
        (Secrets.succeed "https://package.elm-lang.org/search.json")
        (Decode.list Data.packageInfoDecoder)


listModules path =
    List.map (\m -> insertDb m.name "Module" ("/package/" ++ path ++ "/" ++ m.slug ++ "/index.html"))
        >> String.join "\n"


insertDb name type_ path =
    "INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES (\"" ++ name ++ "\", \"" ++ type_ ++ "\" ,\"" ++ path ++ "\");"


routes :
    DataSource (List Route)
    -> (Html Never -> String)
    -> List (ApiRoute.ApiRoute ApiRoute.Response)
routes getStaticRoutes htmlToString =
    [ ApiRoute.succeed
        (packagesDataSource
            |> DataSource.andThen
                (\packages ->
                    packages
                        |> List.map
                            (\{ splittedName } ->
                                modulesDataSource (Tuple.first splittedName) (Tuple.second splittedName)
                            )
                        |> DataSource.combine
                )
            |> DataSource.map
                (\i ->
                    List.map
                        (\{ name, user, modules } ->
                            insertDb (user ++ "/" ++ name)
                                "Package"
                                ("/package/" ++ user ++ "/" ++ name ++ "/index.html")
                                ++ "\n"
                                ++ listModules (user ++ "/" ++ name) modules
                        )
                        i
                        |> String.join "\n"
                        |> (\body ->
                                { body =
                                    "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT);\n"
                                        ++ "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);\n"
                                        ++ body
                                }
                           )
                )
        )
        |> ApiRoute.literal "docs.json"
        |> ApiRoute.single
    ]


myRenderHtmlFunction author package =
    Html.div [] [ Html.text author, Html.text package ]
