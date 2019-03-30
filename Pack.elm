module Main exposing (..)

import Html.Attributes exposing (style)
import Html exposing (..)
import Html.Events exposing (onClick)
import Browser
import Browser.Dom
import Task
import String exposing (fromInt, append)
import Met exposing (getMetObject, Msg(..))
import List exposing (map, range)
import Random
import Time


type Status = Full | NotFull

type alias Rect
  = { x: Int
    , y: Int
    , w: Int
    , h: Int
    }

type alias Model
  = { window : { width: Int, height: Int} 
    , status : Status
    , rects : List Rect }

type Msg
  = Noop
  | AddRect
  | GotViewport (Result () Browser.Dom.Viewport)

roll =
  Random.int 2 4


minHeight = 120
maxHeight = 400


gap = 32
gutter = 8

centredness : Rect -> Float
centredness r = sqrt (toFloat ((r.x + r.w//2)^2 + (r.y + r.h//2)^2))


possibleRects : Model -> Int -> Int -> List Rect
possibleRects model w h =
    let minx = -(model.window.width // 2 // (gutter + gap))
        maxx = -minx - w
        miny = -(model.window.height // 2 // (gutter + gap))
        maxy = -miny - h
    in
      (List.concatMap (\x -> (map (\y -> {x=x, y=y,w=w,h=h})
                                  (range miny maxy)))
            (range minx maxx))



addRect : Model -> Model
addRect model =
    let best = case List.head (List.filter (\x -> (not (List.any (overlap x) model.rects))) (List.sortBy centredness (possibleRects model 4 3))) of
                   Just r -> [r]
                   Nothing -> []
    in { model | rects = model.rects ++ best
        , status = case best of
                              [] -> Full
                              _ -> NotFull
       }


overlap : Rect -> Rect -> Bool
overlap r1 r2 =
    (r1.x+r1.w > r2.x)
     && (r1.x < r2.x+r2.w)
     && (r1.y+r1.h > r2.y)
     && (r1.y < r2.y+r2.h)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Noop ->
      ( model, Cmd.none)
    AddRect ->
      case model.status of
          Full -> ( model, Cmd.none )
          NotFull -> ( addRect model, Cmd.none)
    GotViewport (Ok x) ->
      ( { model | window = { width = (floor x.viewport.width), height = (floor x.viewport.height) }}, Cmd.none )
    GotViewport _ ->
      ( model , Cmd.none )




imageStyle = [ style "width" "30%", style "margin-left" "12px" ]


px : Int -> String
px x
  = (append (fromInt x) "px")

rectScaler : Model -> Rect -> Rect
rectScaler model rect =
    let x = rect.x * (gutter+gap) + (model.window.width//2)
        y = rect.y * (gutter+gap) + (model.window.height//2)
        w = rect.w * gap + (rect.w-1)*gutter
        h = rect.h * gap + (rect.h-1)*gutter
    in { x=x, y=y, w=w, h=h }

drawRect : Rect -> Html Msg
drawRect r =
    div [ style "background" "#222"
        , style "position" "absolute"
        , style "width" (px r.w)
        , style "height" (px r.h)
        , style "left" (px r.x)
        , style "top" (px r.y)
        ] []

drawSpace : Rect -> Html Msg
drawSpace r =
    div [ style "background" "#ccc"
        , style "position" "absolute"
        , style "width" (px r.w)
        , style "height" (px r.h)
        , style "left" (px r.x)
        , style "top" (px r.y)
        ] []

view : Model -> Html Msg
view model =
  div [ onClick AddRect] 
      [ -- div [] (map (drawSpace << rectScaler model) (possibleRects model 1 1))
       div [] (map (drawRect << rectScaler model)  model.rects)
      ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 60 (always AddRect)


initialModel: () -> ( Model, Cmd Msg )
initialModel _ =
  ( { window = {width = 0, height = 0}
    , status = NotFull
    , rects = [{x = -2, y = -2, w = 4, h = 4}] }
  , Task.attempt GotViewport Browser.Dom.getViewport )




main =
  Browser.element
    { init = initialModel
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

