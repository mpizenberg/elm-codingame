module Game exposing (Data)

import Data.Shared exposing (..)
import Data.Terrain exposing (Terrain)


type alias Data =
    { turn : Int
    , gold : Int
    , income : Int
    , enemyGold : Int
    , enemyIncome : Int
    , terrain : Terrain
    , buildings : List Building
    , units : List Unit
    }
