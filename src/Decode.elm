module Decode exposing (gameData, initData)

import Game
import Json.Decode as Decode exposing (Decoder)


initData : Decoder Game.InitData
initData =
    Decode.map3 Game.InitData
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
        (Decode.field "rows" (Decode.list Decode.string))


gameData : Decoder Game.Data
gameData =
    Decode.map7 Game.Data
        (Decode.field "turn" Decode.int)
        (Decode.field "myScore" Decode.int)
        (Decode.field "opponentScore" Decode.int)
        (Decode.field "visiblePacCount" Decode.int)
        (Decode.field "pacs" (Decode.list pacmanDecoder))
        (Decode.field "visiblePelletCount" Decode.int)
        (Decode.field "pellets" (Decode.list pelletDecoder))


pacmanDecoder : Decoder Game.PacmanData
pacmanDecoder =
    Decode.map7 Game.PacmanData
        (Decode.field "pacId" Decode.int)
        (Decode.field "mine" Decode.bool)
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)
        (Decode.field "typeId" Decode.string)
        (Decode.field "speedTurnsLeft" Decode.int)
        (Decode.field "abilityCooldown" Decode.int)


pelletDecoder : Decoder Game.PelletData
pelletDecoder =
    Decode.map3 Game.PelletData
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)
        (Decode.field "value" Decode.int)
