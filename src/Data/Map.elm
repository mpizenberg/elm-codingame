module Data.Map exposing
    ( Map
    , get
    , getDistance1Cells
    , getDistance2Cells
    , hasActiveFriendlyNeighbour
    , isActiveMe
    , isEnemyTower
    , isMyUnit
    , isProtectedByEnemyTower
    , toString
    , update
    , updateBuilding
    , updateUnit
    )

import Array exposing (Array)
import Data.Cell as Cell exposing (Cell)
import Data.Shared exposing (..)


type alias Map =
    Array (Array Cell)



-- Access a cell


get : Int -> Int -> Map -> Maybe Cell
get x y map =
    Array.get y map
        |> Maybe.andThen (Array.get x)


getDistance1Cells : Int -> Int -> Map -> List Cell
getDistance1Cells x y map =
    List.filterMap identity
        [ get x (y - 1) map
        , get (x - 1) y map
        , get (x + 1) y map
        , get x (y + 1) map
        ]


getDistance2Cells : Int -> Int -> Map -> List Cell
getDistance2Cells x y map =
    List.filterMap identity
        [ get x (y - 2) map
        , get (x - 1) (y - 1) map
        , get (x + 1) (y - 1) map
        , get (x - 2) y map
        , get (x + 2) y map
        , get (x - 1) (y + 1) map
        , get (x + 1) (y + 1) map
        , get x (y + 2) map
        ]



-- Update the map


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



-- Check if a cell has some property.
-- Functions of type :
-- Int -> Int -> Map -> Bool


hasActiveFriendlyNeighbour : Int -> Int -> Map -> Bool
hasActiveFriendlyNeighbour x y map =
    isActiveMe x (y - 1) map
        || isActiveMe (x - 1) y map
        || isActiveMe (x + 1) y map
        || isActiveMe x (y + 1) map


isActiveMe : Int -> Int -> Map -> Bool
isActiveMe x y map =
    case get x y map of
        Just (Cell.Active Me _) ->
            True

        _ ->
            False


isMyUnit : Int -> Int -> Map -> Bool
isMyUnit x y map =
    case get x y map of
        Just (Cell.Active Me (Cell.ActiveUnit _)) ->
            True

        _ ->
            False


isProtectedByEnemyTower : Int -> Int -> Map -> Bool
isProtectedByEnemyTower x y map =
    case get x y map of
        Just (Cell.Active Me _) ->
            False

        Just (Cell.Inactive Me _) ->
            False

        _ ->
            isEnemyTower x (y - 1) map
                || isEnemyTower (x - 1) y map
                || isEnemyTower (x + 1) y map
                || isEnemyTower x (y + 1) map


isEnemyTower : Int -> Int -> Map -> Bool
isEnemyTower x y map =
    case get x y map of
        Just (Cell.Active Enemy (Cell.ActiveBuilding building)) ->
            building.type_ == Tower

        _ ->
            False



-- String visualization


toString : Map -> String
toString map =
    Array.toList map
        |> List.map lineAsString
        |> String.join "\n"


lineAsString : Array Cell -> String
lineAsString cells =
    Array.map Cell.toChar cells
        |> Array.toList
        |> String.fromList
