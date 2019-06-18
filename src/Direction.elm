module Direction exposing
    ( Direction(..)
    , fromArrows
    )

import Keyboard.Arrows as Arrows exposing (Direction(..))


type Direction
    = Up
    | Left
    | Down
    | Right


fromArrows : Arrows.Direction -> Maybe Direction
fromArrows arrowDirection =
    case arrowDirection of
        North ->
            Just Up

        NorthEast ->
            Nothing

        East ->
            Just Right

        SouthEast ->
            Nothing

        South ->
            Just Down

        SouthWest ->
            Nothing

        West ->
            Just Left

        NorthWest ->
            Nothing

        NoDirection ->
            Nothing
