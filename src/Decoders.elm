module Decoders exposing (..)

import Json.Decode as JsonD exposing (Decoder, Value, field, int, map6, oneOf, string, succeed)
import Json.Encode as JsonE exposing (Value)

{-| Contains the data from the JWT token we want to have in the application.
-}
type alias JwtToken =
    { id : String
    , login : String
    , nbf : Int
    , exp : Int
    , aud : String
    , displayName : String
    }


tokenJsonDecoder : JsonD.Decoder String
tokenJsonDecoder =
    JsonD.field "accessToken" JsonD.string

reservationIdEncoder : String -> JsonE.Value
reservationIdEncoder id =
    JsonE.object
        [ ("reservationId", JsonE.string id )
        ]

{-| Our default decoder.
-}
tokenDecoder =
    nodeDecoder

{-| Decoder for the JSON.
-}
nodeDecoder =
    map6 JwtToken
        (field "id" string)
        (field "login" string)
        (field "nbf" int)
        (field "exp" int)
        (field "aud" string)
        (field "displayName" string)
