module Decode exposing (gameData, initData)

import Array exposing (Array)
import Data.Cell as Cell exposing (Cell)
import Data.Map exposing (Map)
import Data.Shared exposing (..)
import Game
import Json.Decode as Decode exposing (Decoder, Value)


initData : Decoder (Array Pos)
initData =
    Decode.field "mines" <|
        Decode.map Array.fromList <|
            Decode.list pos


pos : Decoder Pos
pos =
    Decode.map2 Pos
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)


gameData : Decoder Game.Data
gameData =
    Decode.map8 Game.Data
        (Decode.field "turn" Decode.int)
        (Decode.field "gold" Decode.int)
        (Decode.field "income" Decode.int)
        (Decode.field "opponentGold" Decode.int)
        (Decode.field "opponentIncome" Decode.int)
        (Decode.field "terrain" map)
        (Decode.field "buildings" (Decode.list building))
        (Decode.field "units" (Decode.list unit))


map : Decoder Map
map =
    Decode.map Array.fromList mapLine
        |> Decode.array


mapLine : Decoder (List Cell)
mapLine =
    Decode.string
        |> Decode.map String.toList
        |> Decode.andThen (combine << List.map cellHelper)


cellHelper : Char -> Decoder Cell
cellHelper char =
    case Cell.fromChar char of
        Ok cell ->
            Decode.succeed cell

        Err msg ->
            Decode.fail msg


combine : List (Decoder a) -> Decoder (List a)
combine =
    List.foldr (Decode.map2 (::)) (Decode.succeed [])


building : Decoder Building
building =
    Decode.map4 Building
        (Decode.field "owner" owner)
        (Decode.field "buildingType" buildingType)
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)


owner : Decoder Owner
owner =
    Decode.int
        |> Decode.andThen ownerHelper


ownerHelper : Int -> Decoder Owner
ownerHelper n =
    case n of
        0 ->
            Decode.succeed Me

        1 ->
            Decode.succeed Enemy

        _ ->
            Decode.fail "Incorrect owner id"


buildingType : Decoder BuildingType
buildingType =
    Decode.int
        |> Decode.andThen buildingTypeHelper


buildingTypeHelper : Int -> Decoder BuildingType
buildingTypeHelper n =
    case n of
        0 ->
            Decode.succeed Hq

        1 ->
            Decode.succeed Mine

        2 ->
            Decode.succeed Tower

        _ ->
            Decode.fail "Incorrect buildingType id"


unit : Decoder Unit
unit =
    Decode.map5 Unit
        (Decode.field "owner" owner)
        (Decode.field "unitId" Decode.int)
        (Decode.field "level" Decode.int)
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)
