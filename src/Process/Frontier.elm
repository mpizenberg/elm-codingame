module Process.Frontier exposing (compute)

import Data.Cell as Cell exposing (Cell)
import Data.Map as Map exposing (Map)
import Data.Shared exposing (..)
import Dict exposing (Dict)


compute : Map -> Dict ( Int, Int ) Cell
compute map =
    helperFrontier map 0 0 Dict.empty


helperFrontier : Map -> Int -> Int -> Dict ( Int, Int ) Cell -> Dict ( Int, Int ) Cell
helperFrontier map x y cells =
    case ( x, y ) of
        ( _, 12 ) ->
            cells

        ( 12, _ ) ->
            helperFrontier map 0 (y + 1) cells

        _ ->
            case isAtMyFrontier x y map of
                Just cell ->
                    helperFrontier map (x + 1) y (Dict.insert ( x, y ) cell cells)

                Nothing ->
                    helperFrontier map (x + 1) y cells


isAtMyFrontier : Int -> Int -> Map -> Maybe Cell
isAtMyFrontier x y map =
    case Map.getCell x y map of
        Nothing ->
            Nothing

        Just Cell.Void ->
            Nothing

        Just (Cell.Active Me _) ->
            Nothing

        Just cell ->
            if hasActiveFriendlyNeighbour x y map then
                Just cell

            else
                Nothing


hasActiveFriendlyNeighbour : Int -> Int -> Map -> Bool
hasActiveFriendlyNeighbour x y map =
    Map.isActiveMe (x - 1) y map
        || Map.isActiveMe (x + 1) y map
        || Map.isActiveMe x (y - 1) map
        || Map.isActiveMe x (y + 1) map
