module Data.Terrain exposing
    ( Terrain
    , cellsLineString
    , getCell
    , isActiveMe
    , terrainToString
    , update
    , updateBuilding
    , updateUnit
    )

import Array exposing (Array)
import Data.Cell as Cell exposing (Cell)
import Data.Shared exposing (..)


type alias Terrain =
    Array (Array Cell)


updateUnit : Unit -> Terrain -> Terrain
updateUnit u =
    update u.x u.y (Cell.Active u.owner (Cell.ActiveUnit u))


updateBuilding : Building -> Terrain -> Terrain
updateBuilding b terrain =
    case getCell b.x b.y terrain of
        Just (Cell.Active _ _) ->
            update b.x b.y (Cell.Active b.owner (Cell.ActiveBuilding b)) terrain

        _ ->
            update b.x b.y (Cell.Inactive b.owner (Cell.InactiveBuilding b)) terrain


update : Int -> Int -> Cell -> Terrain -> Terrain
update x y cell terrain =
    case Array.get y terrain of
        Nothing ->
            terrain

        Just line ->
            Array.set y (Array.set x cell line) terrain


isActiveMe : Int -> Int -> Terrain -> Bool
isActiveMe x y terrain =
    case getCell x y terrain of
        Just (Cell.Active Me _) ->
            True

        _ ->
            False


getCell : Int -> Int -> Terrain -> Maybe Cell
getCell x y terrain =
    Array.get y terrain
        |> Maybe.andThen (Array.get x)


terrainToString : Terrain -> String
terrainToString terrain =
    Array.toList terrain
        |> List.map cellsLineString
        |> String.join "\n"


cellsLineString : Array Cell -> String
cellsLineString cells =
    Array.map Cell.toChar cells
        |> Array.toList
        |> String.fromList
