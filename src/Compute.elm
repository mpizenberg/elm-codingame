module Compute exposing (myFrontierCells, training)

import Array exposing (Array)
import Dict exposing (Dict)
import Types exposing (..)



-- Training


training : Terrain -> Dict ( Int, Int ) Cell -> List Training
training terrain frontier =
    Dict.foldl (trainingHelper terrain) [] frontier


trainingHelper : Terrain -> ( Int, Int ) -> Cell -> List Training -> List Training
trainingHelper terrain ( x, y ) cell acc =
    case getCell x y terrain of
        Just Neutral ->
            Training 1 x y False :: acc

        Just (Active Enemy (ActiveBuilding building)) ->
            case building.type_ of
                Tower ->
                    acc

                _ ->
                    Training 1 x y True :: acc

        Just (Active Enemy (ActiveUnit unit)) ->
            case unit.level of
                1 ->
                    Training 2 x y False :: acc

                _ ->
                    acc

        Just (Inactive _ (InactiveBuilding building)) ->
            case building.type_ of
                Tower ->
                    acc

                _ ->
                    Training 1 x y True :: acc

        _ ->
            acc



-- Frontier


myFrontierCells : Terrain -> Dict ( Int, Int ) Cell
myFrontierCells terrain =
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
    case getCell x y terrain of
        Nothing ->
            Nothing

        Just Void ->
            Nothing

        Just (Active Me _) ->
            Nothing

        Just cell ->
            if hasActiveFriendlyNeighbour x y terrain then
                Just cell

            else
                Nothing


hasActiveFriendlyNeighbour : Int -> Int -> Terrain -> Bool
hasActiveFriendlyNeighbour x y terrain =
    isActiveMe (x - 1) y terrain
        || isActiveMe (x + 1) y terrain
        || isActiveMe x (y - 1) terrain
        || isActiveMe x (y + 1) terrain
