module Player exposing
    ( Player
    , init
    , move
    )

import Direction exposing (Direction)
import Position exposing (Position)


type alias Player =
    { position : Position
    , direction : Direction
    }


init : Position -> Player
init position =
    Player position Direction.Down


move : Direction -> Player -> Player
move dir player =
    { player
        | direction = dir
        , position = Position.move dir player.position
    }
