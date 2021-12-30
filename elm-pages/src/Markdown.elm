module Markdown exposing (toHtml)

import Html
import Markdown.Parser
import Markdown.Renderer


toHtml attrs markdown =
    let
        deadEndsToString ends =
            ends
                |> List.map Markdown.Parser.deadEndToString
                |> String.join "\n"
    in
    case
        markdown
            |> Markdown.Parser.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer)
    of
        Ok rendered ->
            Html.div [] rendered

        Err error ->
            Html.div [] [ Html.text error ]
