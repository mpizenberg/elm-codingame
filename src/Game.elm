module Game exposing (Data)

import Data.Map exposing (Map)
import Data.Shared exposing (..)


type alias Data =
    { turn : Int
    , gold : Int
    , income : Int
    , enemyGold : Int
    , enemyIncome : Int
    , map : Map
    , buildings : List Building
    , units : List Unit
    }
