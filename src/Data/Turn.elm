module Data.Turn exposing (Data, Pellet, decode)

import Data.Pacman exposing (Pacman)
import Json.Decode as Decode exposing (Decoder)


type alias Data =
    { turn : Int
    , myScore : Int
    , opponentScore : Int
    , visiblePacCount : Int
    , pacs : List Pacman
    , visiblePelletCount : Int
    , pellets : List Pellet
    }


type alias Pellet =
    { x : Int
    , y : Int
    , value : Int
    }


decode : Decoder Data
decode =
    Decode.map7 Data
        (Decode.field "turn" Decode.int)
        (Decode.field "myScore" Decode.int)
        (Decode.field "opponentScore" Decode.int)
        (Decode.field "visiblePacCount" Decode.int)
        (Decode.field "pacs" (Decode.list pacmanDecoder))
        (Decode.field "visiblePelletCount" Decode.int)
        (Decode.field "pellets" (Decode.list pelletDecoder))


pacmanDecoder : Decoder Pacman
pacmanDecoder =
    Decode.map7 Pacman
        (Decode.field "pacId" Decode.int)
        (Decode.field "mine" Decode.bool)
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)
        (Decode.field "typeId" Decode.string)
        (Decode.field "speedTurnsLeft" Decode.int)
        (Decode.field "abilityCooldown" Decode.int)


pelletDecoder : Decoder Pellet
pelletDecoder =
    Decode.map3 Pellet
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)
        (Decode.field "value" Decode.int)
