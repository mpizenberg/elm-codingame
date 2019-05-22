module Data.Map exposing
    ( Map
    , cellsLineString
    , get
    , isActiveMe
    , mapToString
    , update
    , updateBuilding
    , updateUnit
    )

import Array exposing (Array)
import Data.Cell as Cell exposing (Cell)
import Data.Shared exposing (..)


type alias Map =
    Array (Array Cell)


updateUnit : Unit -> Map -> Map
updateUnit u =
    update u.x u.y (Cell.Active u.owner (Cell.ActiveUnit u))


updateBuilding : Building -> Map -> Map
updateBuilding b map =
    case get b.x b.y map of
        Just (Cell.Active _ _) ->
            update b.x b.y (Cell.Active b.owner (Cell.ActiveBuilding b)) map

        _ ->
            update b.x b.y (Cell.Inactive b.owner (Cell.InactiveBuilding b)) map


update : Int -> Int -> Cell -> Map -> Map
update x y cell map =
    case Array.get y map of
        Nothing ->
            map

        Just line ->
            Array.set y (Array.set x cell line) map


isActiveMe : Int -> Int -> Map -> Bool
isActiveMe x y map =
    case get x y map of
        Just (Cell.Active Me _) ->
            True

        _ ->
            False


get : Int -> Int -> Map -> Maybe Cell
get x y map =
    Array.get y map
        |> Maybe.andThen (Array.get x)


mapToString : Map -> String
mapToString map =
    Array.toList map
        |> List.map cellsLineString
        |> String.join "\n"


cellsLineString : Array Cell -> String
cellsLineString cells =
    Array.map Cell.toChar cells
        |> Array.toList
        |> String.fromList
