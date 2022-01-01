module Data exposing (..)

import Dict exposing (Dict)
import OptimizedDecoder as Decode
import OptimizedDecoder.Pipeline as Decode


type alias PackageInfo =
    { name : String
    , splittedName : ( String, String )
    , summary : String
    , version : String
    }


packageInfoDecoder =
    Decode.decode PackageInfo
        |> Decode.required "name" Decode.string
        |> Decode.required "name"
            (Decode.string
                |> Decode.andThen
                    (\name ->
                        case String.split "/" name of
                            [ user, name_ ] ->
                                Decode.succeed ( user, name_ )

                            _ ->
                                Decode.fail <| "Unable to split name for " ++ name
                    )
            )
        |> Decode.required "summary" Decode.string
        |> Decode.required "version" Decode.string


moduleDecoder : Decode.Decoder Module
moduleDecoder =
    Decode.decode Module
        |> Decode.required "name" Decode.string
        |> Decode.required "comment" Decode.string
        |> Decode.required "binops" decodeBinOps
        |> Decode.required "unions" decodeUnions
        |> Decode.required "values" decodeValues
        |> Decode.required "name" (Decode.string |> Decode.map (String.replace "." "-"))


decodeBinOps : Decode.Decoder (Dict String BinOp)
decodeBinOps =
    Decode.list decodeBinOp
        |> Decode.map (List.map (\value -> ( value.name, value )))
        |> Decode.map Dict.fromList


decodeBinOp : Decode.Decoder BinOp
decodeBinOp =
    Decode.decode Value
        |> Decode.required "name" (Decode.map (\binop -> "(" ++ binop ++ ")") Decode.string)
        |> Decode.required "type" Decode.string
        |> Decode.required "comment" Decode.string


decodeValues : Decode.Decoder (Dict String Value)
decodeValues =
    Decode.list decodeValue
        |> Decode.map (List.map (\value -> ( value.name, value )))
        |> Decode.map Dict.fromList


decodeValue : Decode.Decoder Value
decodeValue =
    Decode.decode Value
        |> Decode.required "name" Decode.string
        |> Decode.required "type" Decode.string
        |> Decode.required "comment" Decode.string


decodeNonemptyList =
    Decode.map2 (::)
        (Decode.index 0 Decode.string)
        (Decode.index 1 (Decode.list Decode.string))


decodeUnions : Decode.Decoder (Dict String Union)
decodeUnions =
    Decode.list decodeUnion
        |> Decode.map (List.map (\value -> ( value.name, value )))
        |> Decode.map Dict.fromList


decodeUnion : Decode.Decoder Union
decodeUnion =
    Decode.decode Union
        |> Decode.required "name" Decode.string
        |> Decode.required "comment" Decode.string
        |> Decode.required "args" (Decode.list Decode.string)
        |> Decode.required "cases" (Decode.list decodeNonemptyList)


type alias Module =
    { name : String
    , comment : String
    , binops : Dict String BinOp
    , unions : Dict String Union
    , values : Dict String Value
    , slug : String
    }


type alias Value =
    { name : String
    , type_ : String
    , comment : String
    }


type alias Union =
    { name : String
    , comment : String
    , args : List String
    , cases : List (List String)
    }


type alias BinOp =
    { name : String
    , type_ : String
    , comment : String
    }


type alias Package =
    { info : PackageInfo
    , modules : List Module
    }
