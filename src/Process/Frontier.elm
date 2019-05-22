module Process.Frontier exposing (compute)

import Data.Cell as Cell exposing (Cell)
import Data.Shared exposing (..)
import Data.Terrain as Terrain exposing (Terrain)
import Dict exposing (Dict)


compute : Terrain -> Dict ( Int, Int ) Cell
compute terrain =
    helperFrontier terrain 0 0 Dict.empty


helperFrontier : Terrain -> Int -> Int -> Dict ( Int, Int ) Cell -> Dict ( Int, Int ) Cell
helperFrontier terrain x y cells =
    case ( x, y ) of
        ( _, 12 ) ->
            cells

        ( 12, _ ) ->
            helperFrontier terrain 0 (y + 1) cells

        _ ->
            case isAtMyFrontier x y terrain of
                Just cell ->
                    helperFrontier terrain (x + 1) y (Dict.insert ( x, y ) cell cells)

                Nothing ->
                    helperFrontier terrain (x + 1) y cells


isAtMyFrontier : Int -> Int -> Terrain -> Maybe Cell
isAtMyFrontier x y terrain =
    case Terrain.getCell x y terrain of
        Nothing ->
            Nothing

        Just Cell.Void ->
            Nothing

        Just (Cell.Active Me _) ->
            Nothing

        Just cell ->
            if hasActiveFriendlyNeighbour x y terrain then
                Just cell

            else
                Nothing


hasActiveFriendlyNeighbour : Int -> Int -> Terrain -> Bool
hasActiveFriendlyNeighbour x y terrain =
    Terrain.isActiveMe (x - 1) y terrain
        || Terrain.isActiveMe (x + 1) y terrain
        || Terrain.isActiveMe x (y - 1) terrain
        || Terrain.isActiveMe x (y + 1) terrain
