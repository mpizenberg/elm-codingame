module Strategy.Default exposing (execute)

import Data.State exposing (State)
import Data.Turn as Turn


{-| Default strategy only targets the cell (x=15, y=10) with the first pacman
-}
execute : Turn.Data -> State -> ( State, List ( Int, Int, Int ), Maybe String )
execute data state =
    ( state, [ ( 0, 15, 10 ) ], Nothing )
