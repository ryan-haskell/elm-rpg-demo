module Position exposing (Position, move)

import Direction exposing (Direction(..))


type alias Position =
    { x : Float
    , y : Float
    }


move : Direction -> Position -> Position
move dir position =
    case dir of
        Up ->
            { position | y = position.y - 0.075 }

        Down ->
            { position | y = position.y + 0.075 }

        Left ->
            { position | x = position.x - 0.075 }

        Right ->
            { position | x = position.x + 0.075 }
