module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Direction exposing (Direction(..))
import Keyboard exposing (Key(..))
import Keyboard.Arrows
import Player exposing (Player)
import Position exposing (Position)
import Process
import Svg exposing (..)
import Svg.Attributes as Attr
import Svg.Events as Events
import Task exposing (Task)
import Time


type alias Flags =
    ()


type alias Model =
    { viewport : Maybe Viewport
    , pressedKeys : List Key
    , player : Player
    }


type alias Viewport =
    { width : Int
    , height : Int
    }


sixtyFps =
    1000 / 30


dirToPlayerImage : Direction -> String
dirToPlayerImage dir =
    (\d -> "/public/images/veril/" ++ d ++ "/0.png") <|
        case dir of
            Up ->
                "up"

            Left ->
                "left"

            Right ->
                "right"

            Down ->
                "down"


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


worldSize =
    50


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
        Nothing
        []
        (Player.init
            (Position
                (worldSize / 2)
                (worldSize / 2)
            )
        )
    , Dom.getViewport
        |> Task.map .viewport
        |> Task.map (\{ width, height } -> Viewport (floor width) (floor height))
        |> Task.perform BrowserSentViewport
    )



-- UPDATE


type Msg
    = BrowserSentViewport Viewport
    | KeyboardSentMsg Keyboard.Msg
    | MovePlayer Direction
    | AnimationFrameReceived Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserSentViewport viewport ->
            ( { model | viewport = Just viewport }
            , Cmd.none
            )

        KeyboardSentMsg keyMsg ->
            ( { model
                | pressedKeys = Keyboard.update keyMsg model.pressedKeys
              }
            , Cmd.none
            )

        MovePlayer dir ->
            ( { model | player = Player.move dir model.player }
            , Cmd.none
            )

        AnimationFrameReceived _ ->
            ( Keyboard.Arrows.wasdDirection model.pressedKeys
                |> Direction.fromArrows
                |> Maybe.map (\dir -> Player.move dir model.player)
                |> Maybe.map
                    (\player -> { model | player = player })
                |> Maybe.withDefault model
            , Cmd.none
            )


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.map (always msg)
        |> Task.perform (always msg)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> Viewport w h |> BrowserSentViewport)
        , Browser.Events.onAnimationFrame (always (AnimationFrameReceived 0))
        , Sub.map KeyboardSentMsg Keyboard.subscriptions
        ]



-- VIEW


squares =
    { x = 15
    , y = 9
    }


midpoint =
    { x = squares.x // 2
    , y = squares.y // 2
    }


view : Model -> Svg Msg
view model =
    case model.viewport of
        Just { width, height } ->
            let
                size =
                    Basics.min (width // squares.x) (height // squares.y)

                offsets =
                    { x = (width - (squares.x * size)) // 2
                    , y = (height - (squares.y * size)) // 2
                    }
            in
            Svg.svg
                [ Attr.width (String.fromInt width)
                , Attr.height (String.fromInt height)
                , [ "0", "0", String.fromInt width, String.fromInt height ]
                    |> String.join " "
                    |> Attr.viewBox
                ]
                [ Svg.g
                    [ translate
                        ( toFloat offsets.x - toFloat size * model.player.position.x
                        , toFloat offsets.y - toFloat size * model.player.position.y
                        )
                    ]
                    [ viewTiles size
                    ]
                , Svg.g
                    [ translate
                        ( toFloat offsets.x
                        , toFloat offsets.y
                        )
                    ]
                    [ viewPlayer model.player size
                    ]
                ]

        Nothing ->
            Svg.text ""



-- Shapes


translate : ( Float, Float ) -> Svg.Attribute msg
translate ( x, y ) =
    Attr.transform
        (String.join ""
            [ "translate("
            , String.fromFloat x
            , ","
            , String.fromFloat y
            , ")"
            ]
        )


type alias Size =
    ( Int, Int )


type alias Color =
    String


rectangle : { x : Int, y : Int } -> Size -> Color -> Svg msg
rectangle { x, y } ( w, h ) color =
    rect
        [ Attr.x (String.fromInt (x * w))
        , Attr.y (String.fromInt (y * h))
        , Attr.width (String.fromInt w)
        , Attr.height (String.fromInt h)
        , Attr.fill color
        ]
        []


viewTiles : Int -> Svg msg
viewTiles size =
    Svg.g
        [ Attr.class "tiles"
        ]
        (List.indexedMap (viewTile size)
            (List.map
                (always (List.range 0 worldSize))
                (List.range 0 worldSize)
            )
        )


colors =
    { grass = "#008844"
    , grassDark = "#007733"
    }


viewTile : Int -> Int -> List Int -> Svg msg
viewTile size y indices =
    Svg.g [ Attr.class "tiles__row" ]
        (List.map
            (\x ->
                rectangle
                    { x = x, y = y }
                    ( size, size )
                    (checkered x y colors.grass colors.grassDark)
            )
            indices
        )


checkered : Int -> Int -> String -> String -> String
checkered x y color otherColor =
    if (modBy 2 x + modBy 2 y) == 1 then
        color

    else
        otherColor


viewPlayer : Player -> Int -> Svg msg
viewPlayer { direction } size =
    Svg.image
        [ Attr.xlinkHref (dirToPlayerImage direction)
        , Attr.style "image-rendering: pixelated"
        , Attr.x (String.fromInt (midpoint.x * size))
        , Attr.y (String.fromInt (midpoint.y * size))
        , Attr.width (String.fromInt size)
        , Attr.height (String.fromInt size)
        ]
        []
