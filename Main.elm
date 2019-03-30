module Main exposing (..)

import Html.Attributes exposing (..)
import Html exposing (..)
import Browser
import String
import Met exposing (getMetObject, Msg(..))
import List exposing (map)


ids = [34 , 37]



type Image
  = Failure
  | Loading
  | Success String

type Model
  = List Image


init : Int -> (Image, Cmd Msg)
init id =
  ( Loading
  , getMetObject id
  )

initall : () -> List (Image, Cmd Msg)
initall _ =
    (map init ids)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


update : Msg -> Image -> (Image, Cmd Msg)
update msg model =
  case msg of
    GotText result ->
      case result of
        Ok fullText ->
          (Success fullText, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



imageStyle =
  [ style "width" "30%", style "margin-left" "12px" ]


imageView : Image -> Html Msg
imageView img =
  case img of
    Failure ->
      text "I was unable to find artefact."

    Loading ->
      text "Loading..."

    Success fullText ->
      img ( [ src fullText ] ++ imageStyle ) []



view : Model -> Html Msg
view model =
    div [] (map imageView model)


main =
  Browser.element
    { init = initall
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

