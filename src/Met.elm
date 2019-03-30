module Met exposing (..)


import Http
import Json.Decode exposing (Decoder, field, string, maybe)

api = "https://collectionapi.metmuseum.org/public/collection/v1/objects/"

type Msg
  = GotText (Result Http.Error String)

type alias ID = Int

getMetObject : ID -> Cmd Msg
getMetObject id =
  Http.get
    { url = String.join "" [ api , (String.fromInt id) ]
    , expect = Http.expectJson GotText metDecoder
    }

metDecoder : Decoder String
metDecoder =
  field "primaryImageSmall" string
