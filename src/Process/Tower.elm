module Process.Tower exposing (TowerScore, order, sortedList, toString)

import Data.Cell as Cell exposing (Cell)
import Data.CostMap as CostMap exposing (CostMap)
import Data.Map as Map exposing (Map)
import Data.Shared exposing (..)
import Set exposing (Set)


type alias TowerScore =
    { x : Int
    , y : Int
    , towerScore : Int
    }


order : TowerScore -> String
order { x, y } =
    "BUILD TOWER " ++ String.fromInt x ++ " " ++ String.fromInt y


toString : TowerScore -> String
toString { x, y, towerScore } =
    String.join " "
        [ "Tower score:"
        , String.fromInt x
        , String.fromInt y
        , String.fromInt towerScore
        ]


sortedList : Set Node -> CostMap -> CostMap -> Map -> List TowerScore
sortedList minesSpots costMap criticalMap map =
    Map.foldl (buildAcc minesSpots costMap criticalMap map) [] map
        |> List.sortBy (\t -> t.towerScore)


buildAcc : Set Node -> CostMap -> CostMap -> Map -> Int -> Int -> Cell -> List TowerScore -> List TowerScore
buildAcc minesSpots costMap criticalMap map x y cell list =
    if Set.member ( x, y ) minesSpots then
        list

    else
        case Map.get x y map of
            Just (Cell.Active Me Cell.ActiveNothing) ->
                TowerScore x y (score costMap criticalMap map x y) :: list

            _ ->
                list


score : CostMap -> CostMap -> Map -> Int -> Int -> Int
score costMap criticalMap map x y =
    let
        d1Score =
            List.sum (List.map baseScoreD1 (Map.getDistance1Cells x y map))

        d2Score =
            List.sum (List.map baseScoreD2 (Map.getDistance2Cells x y map))

        distance =
            CostMap.get x y costMap

        critical =
            CostMap.get x y criticalMap

        spread =
            abs (x - y)
    in
    -- Negate because of increase sort order
    -(2 * d1Score + d2Score - distance + critical - spread)


baseScoreD1 : Cell -> Int
baseScoreD1 cell =
    case cell of
        Cell.Void ->
            0

        Cell.Neutral ->
            1

        Cell.Active Me (Cell.ActiveBuilding building) ->
            case building.type_ of
                Tower ->
                    -3

                _ ->
                    1

        Cell.Active Me _ ->
            2

        Cell.Active Enemy (Cell.ActiveUnit unit) ->
            case unit.level of
                3 ->
                    -5

                2 ->
                    2

                _ ->
                    1

        Cell.Active Enemy _ ->
            1

        Cell.Inactive _ _ ->
            1


baseScoreD2 : Cell -> Int
baseScoreD2 cell =
    case cell of
        Cell.Void ->
            0

        Cell.Neutral ->
            2

        Cell.Active Me (Cell.ActiveBuilding building) ->
            case building.type_ of
                Tower ->
                    -1

                _ ->
                    1

        Cell.Active Me _ ->
            1

        Cell.Active Enemy (Cell.ActiveUnit unit) ->
            case unit.level of
                3 ->
                    -1

                2 ->
                    4

                _ ->
                    3

        Cell.Active Enemy _ ->
            2

        Cell.Inactive _ _ ->
            2
