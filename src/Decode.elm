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
            Decode.list posDecoder


posDecoder : Decoder Pos
posDecoder =
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
        (Decode.field "terrain" mapDecoder)
        (Decode.field "buildings" (Decode.list buildingDecoder))
        (Decode.field "units" (Decode.list unitDecoder))


mapDecoder : Decoder Map
mapDecoder =
    Decode.map Array.fromList help1
        |> Decode.array


help1 : Decoder (List Cell)
help1 =
    Decode.string
        |> Decode.map String.toList
        |> Decode.andThen help2


help2 : List Char -> Decoder (List Cell)
help2 chars =
    List.map cellDecoderHelper chars
        |> decodeCombine


cellDecoderHelper : Char -> Decoder Cell
cellDecoderHelper char =
    case Cell.fromChar char of
        Ok cell ->
            Decode.succeed cell

        Err msg ->
            Decode.fail msg


decodeCombine : List (Decoder a) -> Decoder (List a)
decodeCombine =
    List.foldr (Decode.map2 (::)) (Decode.succeed [])


buildingDecoder : Decoder Building
buildingDecoder =
    Decode.map4 Building
        (Decode.field "owner" ownerDecoder)
        (Decode.field "buildingType" buildingTypeDecoder)
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)


ownerDecoder : Decoder Owner
ownerDecoder =
    Decode.int
        |> Decode.andThen ownerDecoderHelper


ownerDecoderHelper : Int -> Decoder Owner
ownerDecoderHelper n =
    case n of
        0 ->
            Decode.succeed Me

        1 ->
            Decode.succeed Enemy

        _ ->
            Decode.fail "Incorrect owner id"


buildingTypeDecoder : Decoder BuildingType
buildingTypeDecoder =
    Decode.int
        |> Decode.andThen buildingTypeDecoderHelper


buildingTypeDecoderHelper : Int -> Decoder BuildingType
buildingTypeDecoderHelper n =
    case n of
        0 ->
            Decode.succeed Hq

        1 ->
            Decode.succeed Mine

        2 ->
            Decode.succeed Tower

        _ ->
            Decode.fail "Incorrect buildingType id"


unitDecoder : Decoder Unit
unitDecoder =
    Decode.map5 Unit
        (Decode.field "owner" ownerDecoder)
        (Decode.field "unitId" Decode.int)
        (Decode.field "level" Decode.int)
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)
