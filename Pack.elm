module Main exposing (..)

import Html.Attributes exposing (style)
import Html exposing (..)
import Html.Events exposing (onClick)
import Browser
import Browser.Dom
import Task
import String exposing (fromInt, append, join)
-- import NMA exposing (getNMAObject, Msg(..))
import List exposing (map, range)
import Random
import Time

import Html.Events.Extra.Mouse as Mouse

import Http
import Json.Decode exposing (Decoder, field, string, map4, int)

api = "http://localhost:3000/"


getNMAObject : Int -> Int -> Cmd Msg
getNMAObject x y =
  Http.get
    { url = api
    , expect = Http.expectJson GotJson nmaDecoder
    }

nmaDecoder : Decoder UnplacedRect
nmaDecoder =
    map4 UnplacedRect
    ( field "w" int )
    ( field "h" int )
    ( field "url" string )
    ( field "color" string )


type Status = Full | NotFull

type alias Rect
  = { x: Int
    , y: Int
    , w: Int
    , h: Int
    , url: String
    , color: String
    }

type alias UnplacedRect
  = { w: Int
    , h: Int
    , url: String
    , color: String
    }


type alias Model
  = { window : { width: Int, height: Int}
    , status : Status
    , loc : { x: Float, y: Float}
    , mouse : { x: Float, y: Float}
    , rects : List Rect }

type Msg
  = Noop
  | AddRect
  | GotViewport (Result () Browser.Dom.Viewport)
  | MouseMove ( Float, Float )
  | Move
  | GotJson (Result Http.Error UnplacedRect)



gap = 32
gutter = 8

centredness : Rect -> Float
centredness r = sqrt (toFloat ((r.x + r.w//2)^2 + (r.y + r.h//2)^2))


possibleRects : Model -> UnplacedRect -> List Rect
possibleRects model new =
    let minx = -(model.window.width // 2 // (gutter + gap))
        maxx = -minx - new.w
        miny = -(model.window.height // 2 // (gutter + gap))
        maxy = -miny - new.h
    in
      (List.concatMap (\x -> (map (\y -> {  x=x, y=y, w=new.w, h=new.h, color=new.color, url=new.url })
                                  (map ((+) ((round model.loc.y) // 40)) (range miny maxy))))
            (map ((+) ((round model.loc.x) // 40)) (range minx maxx)))



addRect : UnplacedRect -> Model -> Model
addRect new model =
    let best = case List.head (List.filter (\x -> (not (List.any (overlap x) model.rects))) (List.sortBy centredness (possibleRects model new))) of
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
          NotFull -> ( model, getNMAObject 4 4 )
    GotViewport (Ok x) ->
      ( { model | window = { width = (floor x.viewport.width), height = (floor x.viewport.height) }}, Cmd.none )
    GotViewport _ ->
      ( model , Cmd.none )
    MouseMove (x, y) ->
      ( {model | mouse = { x = (x - (toFloat model.window.width) / 2), y = (y - (toFloat model.window.height) / 2)}, status = NotFull}, Cmd.none )
    Move ->
        let pow = (sqrt (model.mouse.x^2 + model.mouse.y^2) - 100) / 10000
        in
          case pow > 0 of
              True ->
                ( {model | loc = { x = model.loc.x + model.mouse.x * pow, y = model.loc.y + model.mouse.y * pow * 2.5}, status = NotFull}, Cmd.none )
              False ->
                ( model , Cmd.none )
    GotJson result ->
        case result of
            Ok newImg ->
                ( addRect newImg model, Cmd.none )
            Err _ ->
                ( model , Cmd.none )






imageStyle = [ style "width" "30%", style "margin-left" "12px" ]


px : Int -> String
px x
  = (append (fromInt x) "px")

rectScaler : Model -> Rect -> Rect
rectScaler model rect =
    let x = rect.x * (gutter+gap) + (model.window.width//2) - (round model.loc.x)
        y = rect.y * (gutter+gap) + (model.window.height//2) - (round model.loc.y)
        w = rect.w * gap + (rect.w-1)*gutter
        h = rect.h * gap + (rect.h-1)*gutter
    in { x=x, y=y, w=w, h=h, url=rect.url, color=rect.color }

drawRect : Rect -> Html Msg
drawRect r =
    div [ style "background-color" r.color
        , style "position" "absolute"
        , style "width" (px r.w)
        , style "height" (px r.h)
        , style "left" (px r.x)
        , style "top" (px r.y)
        , style "background-image" (String.join "" [ "url(", r.url,  ")" ])
        , style "background-size" "cover"
        , style "background-position" "center"
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
  div [ style "width" "100vw", style "height" "100vh", style "position" "relative",  Mouse.onMove (\event -> MouseMove event.screenPos), onClick AddRect]
      [ -- div [] (map (drawSpace << rectScaler model) (possibleRects model 1 1))
       div [] (map (drawRect << rectScaler model)  model.rects)
      ]


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Time.every 40 (always Move)
            , Time.every 200 (always AddRect)
         --, Time.every 100 (always (getNMAObject 4 4))
            ]


initialModel: () -> ( Model, Cmd Msg )
initialModel _ =
  ( { window = {width = 0, height = 0}
    , status = NotFull
    , loc = { x = 0, y = 0}
    , mouse = { x = 0, y = 0}
    , rects = [{x = -2, y = -2, w = 4, h = 4, color = "#f00", url="http://collectionsearch.nma.gov.au/nmacs-image-download/piction/dams_data/prodderivW/DAMS_INGEST/JOBS/WM_60618335/nma_60672802.jpg"}] }
  , Cmd.batch [
         Task.attempt GotViewport Browser.Dom.getViewport
        , getNMAObject 4 4
        ])



main =
  Browser.element
    { init = initialModel
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
