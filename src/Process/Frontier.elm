module Process.Frontier exposing (compute)

import Data.Cell as Cell exposing (Cell)
import Data.Map as Map exposing (Map)
import Data.Shared exposing (..)
import Dict exposing (Dict)


compute : Map -> Dict ( Int, Int ) Cell
compute map =
    helper map 0 0 Dict.empty


helper : Map -> Int -> Int -> Dict ( Int, Int ) Cell -> Dict ( Int, Int ) Cell
helper map x y cells =
    case ( x, y ) of
        ( _, 12 ) ->
            cells

        ( 12, _ ) ->
            helper map 0 (y + 1) cells

        _ ->
            case contains x y map of
                Just cell ->
                    helper map (x + 1) y (Dict.insert ( x, y ) cell cells)

                Nothing ->
                    helper map (x + 1) y cells


contains : Int -> Int -> Map -> Maybe Cell
contains x y map =
    case Map.get x y map of
        Nothing ->
            Nothing

        Just Cell.Void ->
            Nothing

        Just (Cell.Active Me _) ->
            Nothing

        Just cell ->
            if Map.hasActiveFriendlyNeighbour x y map then
                Just cell

            else
                Nothing
