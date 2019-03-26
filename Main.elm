module Main exposing (..)

import Html.Attributes exposing (..)
import Html exposing (..)
import Browser
import String
import Met exposing (getMetObject, Msg(..))


ids = [34 , 1]




type Model
  = Failure
  | Loading
  | Success String


init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , getMetObject 37
  )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
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

view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "I was unable to find artefact."

    Loading ->
      text "Loading..."

    Success fullText ->
      pre [] [
             img ( [ src fullText ] ++ imageStyle ) []
           , img ( [ src fullText ] ++ imageStyle ) []
          ]


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

