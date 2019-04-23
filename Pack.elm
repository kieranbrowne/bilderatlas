module Main exposing (..)

import Html.Attributes exposing (style, href)
import Html exposing (..)
import Html.Events exposing (onClick)
import Browser
import Browser.Dom
import Task
import String exposing (fromInt, append, join)
-- import NMA exposing (getNMAObject, Msg(..))
import List exposing (map, range)
import Array exposing (Array)
import Random
import Time

import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Html.Events exposing (on)

import Http
import Json.Decode as D exposing (Decoder, field, string, map5, int)

api = "https://kieranbrowne.com/infinite-salon/data/"


getNMAObject : String -> Cmd Msg
getNMAObject id =
  Http.get
    { url = String.join "" [api , "object/" , id]
    , expect = Http.expectJson GotJson nmaObjectDecoder
    }

nmaObjectDecoder : Decoder UnplacedRect
nmaObjectDecoder =
    map5 UnplacedRect
    ( field "w" int )
    ( field "h" int )
    ( field "url" string )
    ( field "color" string )
    ( field "id" string )

getNMAOptions : Cmd Msg
getNMAOptions =
  Http.get
    { url = String.join "" [api , "options"]
    , expect = Http.expectJson GotOptions nmaOptionsDecoder
    }

nmaOptionsDecoder : Decoder (List String)
nmaOptionsDecoder =
    D.list D.string


type Status = Full | NotFull


type alias Rect
  = { x: Int
    , y: Int
    , w: Int
    , h: Int
    , url: String
    , color: String
    , id: String
    }

type alias UnplacedRect
  = { w: Int
    , h: Int
    , url: String
    , color: String
    , id: String
    }

type Touch
  = Up
  | Down

type alias Model
  = { window : { width: Int, height: Int}
    , status : Status
    , loc : { x: Float, y: Float}
    , storedloc : { x: Float, y: Float}
    , mouse : { x: Float, y: Float}
    , rects : List Rect
    , pick : Int
    , options : Array String
    , touch : Touch
    }

type Msg
  = Noop
  | AddRect
  | GotViewport (Result () Browser.Dom.Viewport)
  | MouseMove ( Float, Float )
  | Move
  | TouchStart ( Float, Float )
  | TouchEnd ( Float, Float )
  | TouchMove ( Float, Float )
  | GotJson (Result Http.Error UnplacedRect)
  | GotOptions (Result Http.Error (List String))
  | RandomPick Int



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
      (List.concatMap (\x -> (map (\y -> {  x=x, y=y, w=new.w, h=new.h, color=new.color, url=new.url, id=new.id })
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

    RandomPick pick ->
        ( {model | pick = pick}, Cmd.none )

    AddRect ->
      case model.status of
          Full -> ( model, Cmd.none )
          NotFull ->
              let id = case (Array.get model.pick model.options) of
                           Just x -> x
                           Nothing -> "111093"
              in ( model, Cmd.batch [getNMAObject id , Random.generate RandomPick (Random.int 0 (Array.length model.options))] )
    GotViewport (Ok x) ->
      ( { model | window = { width = (floor x.viewport.width), height = (floor x.viewport.height) }}, Cmd.none )
    GotViewport _ ->
      ( model , Cmd.none )

    MouseMove (x, y) ->
      ( {model | mouse = { x = (x - (toFloat model.window.width) / 2), y = (y - (toFloat model.window.height) / 2)}, status = NotFull}, Cmd.none )


    Move ->
        case model.touch of
            Up ->
              let pow = (sqrt (model.mouse.x^2 + model.mouse.y^2) - 100) / 10000
              in
                case pow > 0 of
                    True ->
                      ( {model | loc = { x = model.loc.x + model.mouse.x * pow, y = model.loc.y + model.mouse.y * pow * 2.5}, status = NotFull}, Cmd.none )
                    False ->
                        (model, Cmd.none)
            Down ->
                (model, Cmd.none)

    TouchStart (x,y) ->
      ( { model | touch = Down
        , mouse = {x= x, y= y}
        , storedloc = {x= model.loc.x, y = model.loc.y}
        } , Cmd.none )
    TouchEnd (x,y) ->
      ( { model | touch = Up
        , mouse = {x=0,y=0}} , Cmd.none )
    TouchMove (x, y) ->
        ( {model
              | loc = { x = model.storedloc.x - (x-model.mouse.x)/1
                      , y = model.storedloc.y - (y-model.mouse.y)/1 }
              , status = NotFull}, Cmd.none )

    GotJson result ->
        case result of
            Ok newImg ->
                ( addRect newImg model, Cmd.none )
            Err _ ->
                ( model , Cmd.none )

    GotOptions result ->
        case result of
            Ok newOptions ->
                ( {model | options = Array.fromList newOptions}, Cmd.none )
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
    in { x=x, y=y, w=w, h=h, url=rect.url, color=rect.color , id=rect.id}

drawRect : Rect -> Html Msg
drawRect r =
    a [ href (String.join "" [ "http://collectionsearch.nma.gov.au/object/", r.id ]) ]
        [ div [ style "background-color" r.color
              , style "position" "absolute"
              , style "width" (px r.w)
              , style "height" (px r.h)
              , style "left" (px r.x)
              , style "top" (px r.y)
              , style "background-image" (String.join "" [ "url(", r.url,  ")" ])
              , style "background-size" "cover"
              , style "background-position" "center"
              ] [] ]

drawSpace : Rect -> Html Msg
drawSpace r =
    div [ style "background" "#ccc"
        , style "position" "absolute"
        , style "width" (px r.w)
        , style "height" (px r.h)
        , style "left" (px r.x)
        , style "top" (px r.y)
        ] []


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


view : Model -> Html Msg
view model =
  case (List.length model.rects) > 0 of
      True ->
        div [ style "width" "100vw"
            , style "height" "100vh"
            , style "position" "relative"
            , Mouse.onMove (\event -> MouseMove event.screenPos)
            --, onClick AddRect
            , Touch.onStart (TouchStart << touchCoordinates)
            , Touch.onEnd (TouchEnd << touchCoordinates)
            , Touch.onMove (TouchMove << touchCoordinates)
            ]
            [-- div [] (map (drawSpace << rectScaler model) (possibleRects model {w=1,h=1,url="",color=""})) ,
            div [] (map (drawRect << rectScaler model)  model.rects)
            ]
      False ->
           div [] [text "loading"]


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
    , storedloc = { x = 0, y = 0}
    , mouse = { x = 0, y = 0}
    , rects = []
    , pick = 0
    , touch = Up
    , options = Array.fromList []}
  , Cmd.batch [
         Task.attempt GotViewport Browser.Dom.getViewport
        --, getNMAObject "111093"
        , getNMAOptions
        , Random.generate RandomPick (Random.int 0 10)
        ])



main =
  Browser.element
    { init = initialModel
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
