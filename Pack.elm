module Main exposing (..)

import Html.Attributes exposing (style, href, contenteditable, size, value, placeholder)
import Html exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Browser
import Browser.Dom
import Task
import String exposing (fromInt, append, join)
-- import NMA exposing (getNMAObject, Msg(..))
import List exposing (map, range)
import Array exposing (Array)
import Random
import Time
import Browser.Navigation
import Debug exposing (toString)

import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Html.Events exposing (on)

import Http
import Json.Decode as D exposing (Decoder, field)

api = "//kieranbrowne.com/nma-explorer/data/"


options = ["pier", "envelope", "menu", "coffee mug"]


getNMAObject : String -> Cmd Msg
getNMAObject id =
  Http.get
    { url = String.join "" [api , "object/" , id]
    , expect = Http.expectJson GotJson nmaObjectDecoder
    }

nmaObjectDecoder : Decoder UnplacedRect
nmaObjectDecoder =
    D.map6 UnplacedRect
    ( field "w" D.int )
    ( field "h" D.int )
    ( field "url" D.string )
    ( field "color" D.string )
    ( field "id" D.string )
    ( field "closest" (D.list D.string) )

getNMAOptions : Cmd Msg
getNMAOptions =
  Http.get
    { url = String.join "" [api , "options"]
    , expect = Http.expectJson GotOptions nmaOptionsDecoder
    }

getNMACategories : Cmd Msg
getNMACategories =
  Http.get
    { url = String.join "" [api , "category_list"]
    , expect = Http.expectJson GotCategories nmaOptionsDecoder
    }

getNMACategory : String -> Cmd Msg
getNMACategory query =
  Http.get
    { url = String.join "" [api , "categories/" , String.replace " " "_" query]
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
    , closest: List String
    }

type alias UnplacedRect
  = { w: Int
    , h: Int
    , url: String
    , color: String
    , id: String
    , closest: List String
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
    , query : String
    , rects : List Rect
    , catpick : Int
    , pick : Int
    , options : Array String
    , categories : List String
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
  | GotCategories (Result Http.Error (List String))
  | RandomPick Int
  | NewQuery String
  | SubmitQuery


gap = 38
gutter = 8
block = gap + gutter

centredness : Rect -> Float
centredness r = sqrt (toFloat ((r.x + r.w//2)^2 + (r.y + r.h//2)^2))


possibleRects : Model -> UnplacedRect -> List Rect
possibleRects model new =
    let minx = -(model.window.width // 2 // (gutter + gap))
        maxx = -minx - new.w
        miny = -(model.window.height // 2 // (gutter + gap))
        maxy = -miny - new.h
    in
      (List.concatMap (\x -> (map (\y -> { x=x, y=y, w=new.w, h=new.h, color=new.color, url=new.url, id=new.id, closest=new.closest })
                                  (map ((+) ((round model.loc.y) // block)) (range miny maxy))))
            (map ((+) ((round model.loc.x) // block)) (range minx maxx)))

withinRange : Model -> Model
withinRange model =
    let minx = -(model.window.width // 2 // (gutter + gap))
        maxx = -minx - 5
        miny = -(model.window.height // 2 // (gutter + gap))
        maxy = -miny - 5
        x = ((round model.loc.x) // block)
        y = ((round model.loc.y) // block)
    in
        {model | rects = List.filter (\r -> r.x+x > minx && r.x+x < maxx && r.y+y > miny && r.y+y < maxy) model.rects}


nextID : Model -> Maybe String
nextID model =
    let sorted =
            (List.sortBy centredness
                 (map (\r -> {r | x=r.x + ((round model.loc.x) // block)
                             , y=r.y + ((round model.loc.y) // block)
                             }) model.rects))
        best =
            case (List.head sorted) of
                Just x -> x
                Nothing -> {x=0,y=0,w=0,h=0,url="",color="",id="",closest=[]}
        others =
            case (List.tail sorted) of
                Just x -> x
                Nothing -> []
        remaining =
            case List.tail best.closest of
                Just x -> x
                Nothing -> []
        c =
            case List.head best.closest of
                Just x -> x
                Nothing -> "NULL"
    in case List.isEmpty best.closest of
        True ->
            let submodel = {model | rects=List.filter (\r -> r /= best) model.rects}
            in
              case List.isEmpty submodel.rects of
                  True -> Array.get model.pick model.options
                  False -> nextID submodel
        _ ->
            case List.member c (Array.toList model.options) of
                True -> Just c
                False -> let newmodel = case remaining of
                                []  -> { model | rects = others}
                                _ -> { model | rects = {best|closest = remaining} :: others}
                         in nextID newmodel



addRect : UnplacedRect -> Model -> Model
addRect new model =
    let best = case List.head (List.filter (\x -> (not (List.any (overlap x) model.rects))) (List.sortBy centredness (possibleRects model new))) of
                   Just r -> [r]
                   Nothing -> []
    in { model | rects = model.rects ++ best
        , status =
             case best of
                 [] -> Full
                 _ -> NotFull
       }

overlap : Rect -> Rect -> Bool
overlap r1 r2 =
    (r1.x+r1.w > r2.x)
     && (r1.x < r2.x+r2.w)
     && (r1.y+r1.h > r2.y)
     && (r1.y < r2.y+r2.h)


similar : Model -> Model -> String
similar model reducer =
    case List.head (List.filter (\x -> not (List.isEmpty x.closest)) reducer.rects) of
        Just rect ->
            case List.head rect.closest of
                Just id ->
                    case (List.member id (map (\x -> x.id) model.rects)) of
                        True ->
                            similar model
                                {reducer | rects =
                                     (List.map
                                          (\r -> {r | closest =
                                               (List.filter
                                                    (\x -> not (List.member x (map (\y -> y.id) model.rects)))
                                                         r.closest)})
                                          reducer.rects)}
                        False -> id
                Nothing -> ""
        Nothing -> ""

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Noop ->
      ( model, Cmd.none)

    NewQuery txt ->
        ( {model | query = txt}, Cmd.none )

    SubmitQuery ->
        ( {model | query = model.query ++ complete model}, getNMACategory (model.query ++ complete model))

    RandomPick pick ->
        ( {model | pick = pick}, Cmd.none )

    AddRect ->
      case model.status of
          Full -> ( model, Cmd.none )
          NotFull ->
              let id = case Array.get model.pick model.options of
                           Just x -> x
                           Nothing -> similar model model
              in ( {model | options = Array.filter (\x -> x /= id) model.options }, Cmd.batch [ getNMAObject id
                                                                                              , Random.generate RandomPick (Random.int 0 (Array.length model.options))
                                                                                              ] )
    GotViewport (Ok x) ->
      ( { model | window = { width = (floor x.viewport.width), height = (floor x.viewport.height) }}, Cmd.none )
    GotViewport _ ->
      ( model , Cmd.none )

    MouseMove (x, y) ->
      ( {model | mouse = { x = (x - (toFloat model.window.width) / 2), y = (y - (toFloat model.window.height) / 2)}, status = NotFull}, Cmd.none )


    Move ->
        case List.length model.rects > 1 of
            False -> (model, Cmd.none)
            True ->
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
                let nextid =
                        case Array.get model.pick (Array.fromList newOptions) of
                            Just x -> x
                            Nothing -> ""
                in
                  ( {model | options = Array.fromList (List.filter (\x -> x /= nextid) newOptions)}, getNMAObject nextid )
            Err _ ->
                ( model , Cmd.none )

    GotCategories result ->
        case result of
            Ok cats ->
                ( {model | categories = map (String.replace "_" " ") cats }, Cmd.none )
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
    in { rect | x=x, y=y, w=w, h=h}

targetTextContent : Decoder String
targetTextContent =
  D.at ["target", "textContent"] D.string

complete : Model -> String
complete model =
    case model.query of
        "" -> ""
        _ ->
          case List.head (List.filter (String.startsWith model.query) model.categories) of
              Just s -> String.slice (String.length model.query) 99 s
              Nothing -> ""

drawRect : Model -> Rect -> Html Msg
drawRect model r =
    case r.color of
        "#ddd" ->
            let w = case model.query of
                    "" -> "100%"
                    _ -> toString (toFloat (String.length model.query)*9.6) ++ "px"
                ib = case model.query of
                    "" -> "inline-block"
                    _ -> "inline"


            in
              div [
                style "background-color" r.color
                , style "position" "absolute"
                , style "appearance" "none"
                , style "border" "none"
                , style "padding-left" ".5em"
                , style "padding-right" ".5em"
                , style "box-sizing" "border-box"
                , style "text-align" "center"
                , style "font-size" "1rem"
                , style "font-family" "monospace"
                , style "width" (px r.w)
                , style "height" (px r.h)
                , style "line-height" (px r.h)
                , style "left" (px r.x)
                , style "top" (px r.y)
                , style "background-size" "cover"
                , style "background-position" "center"
                ] [ form [ onSubmit SubmitQuery, style "display" "inline" ]
                        [ input [ contenteditable True
                                , style "width" w
                                , style "height" "auto"
                                , style "appearance" "none"
                                , style "font-family" "monospace"
                                , style "font-size" "1rem"
                                , style "border" "none"
                                , style "background" "none"
                                , style "background" "none"
                                , style "display" ib
                                , style "text-align" "center"
                                , size 0
                                , on "keyup" (D.map NewQuery targetTextContent)
                                , onInput NewQuery
                                , placeholder (String.join "" ["e.g. ", Maybe.withDefault "mask" (Array.get (modBy (List.length model.categories) model.catpick) (Array.fromList model.categories)), "..."])
                                , value model.query
                          ] [ text model.query ] ] ,
                    span [style "color" "#aaa"] [ text (complete model)]
                  ]
        _ ->
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
            , Touch.onStart (TouchStart << touchCoordinates)
            , Touch.onEnd (TouchEnd << touchCoordinates)
            , Touch.onMove (TouchMove << touchCoordinates)
            ]
            [ --div [] (map (drawSpace << rectScaler model) (possibleRects model {w=1,h=1,url="",color="", id="", closest=[]})) ,
            div []
                [ --text (toString (Array.length model.options))
                --,
                    div [] (map (drawRect model << rectScaler model)  model.rects)]
            ]
      False ->
           div [] [text "loading"]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 40 (always Move)
        , Time.every 500 (always AddRect)
        ]


initialModel: Int -> ( Model, Cmd Msg )
initialModel flags =
  ( { window = {width = 0, height = 0}
    , status = NotFull
    , loc = { x = 0, y = 0}
    , storedloc = { x = 0, y = 0}
    , mouse = { x = 0, y = 0}
    , rects = [{x=-4,y=-1,w=8,h=1,color="#ddd",id="",url="",closest=[]}]
    , query = ""
    , catpick = flags
    , pick = 0
    , touch = Up
    , categories = ["mask"]
    , options = Array.fromList []}
  , Cmd.batch [
         Task.attempt GotViewport Browser.Dom.getViewport
        --, getNMAObject "111093"
        , getNMACategories
        , Random.generate RandomPick (Random.int 0 3000)
        ])



main =
  Browser.element
    { init = initialModel
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
