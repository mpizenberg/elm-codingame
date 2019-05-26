module Process.Training exposing
    ( Training
    , compute
    , order
    , sort
    , spend
    )

import Data.Cell as Cell exposing (Cell)
import Data.CostMap as CostMap exposing (CostMap)
import Data.Map as Map exposing (Map)
import Data.Shared exposing (..)
import Dict exposing (Dict)


type alias Training =
    { level : Int
    , x : Int
    , y : Int
    , cell : Cell
    }


type Strat
    = Greedy
    | Protective


order : { a | level : Int, x : Int, y : Int } -> String
order { level, x, y } =
    String.join " " ("TRAIN" :: List.map String.fromInt [ level, x, y ])



-- Spend all our money on trainings ;)
-- Last argument is a loop accumulator, should be called with [] initially.


spend : Int -> List Training -> List Training -> List Training
spend gold list acc =
    case list of
        [] ->
            acc

        x :: xs ->
            let
                newGold =
                    gold - 10 * x.level
            in
            if newGold >= 0 then
                spend newGold xs (x :: acc)

            else
                acc



-- Sort trainings in such a way that we prioritise
-- the most cost-effective ones.


sort : CostMap -> CostMap -> Map -> List Training -> List Training
sort costMap criticalMap map =
    List.sortBy (score costMap criticalMap map)


score : CostMap -> CostMap -> Map -> Training -> Int
score costMap criticalMap map t =
    let
        targetScore =
            baseScore t.cell

        d1Score =
            List.sum (List.map baseScore (Map.getDistance1Cells t.x t.y map))

        d2Score =
            List.sum (List.map baseScore (Map.getDistance2Cells t.x t.y map))

        levelScore =
            4 - t.level

        distance =
            CostMap.get t.x t.y costMap

        critical =
            2 * CostMap.get t.x t.y criticalMap

        spread =
            2 * abs (t.x - t.y)
    in
    -- Negate because of increase sort order
    -levelScore * (8 * targetScore + 2 * d1Score + d2Score + distance + critical - spread)


baseScore : Cell -> Int
baseScore cell =
    case cell of
        Cell.Neutral ->
            1

        Cell.Inactive Enemy Cell.InactiveNothing ->
            2

        Cell.Active Enemy Cell.ActiveNothing ->
            2

        Cell.Active Enemy (Cell.ActiveUnit _) ->
            2

        Cell.Inactive _ (Cell.InactiveBuilding building) ->
            case building.type_ of
                Tower ->
                    4

                Mine ->
                    5

                Hq ->
                    10

        Cell.Active Enemy (Cell.ActiveBuilding building) ->
            case building.type_ of
                Tower ->
                    5

                Mine ->
                    6

                Hq ->
                    10

        -- We want negative score on our Active units (not too crowded for expansion).
        Cell.Active Me (Cell.ActiveUnit _) ->
            -1

        -- Actually if we are in risky area (confrontation), we should stay near friendly cells.
        -- So maybe we could have a mode.
        --     - greedy -> -1
        --     - defensive -> +1
        Cell.Active Me _ ->
            -1

        -- If we have Inactive cells in neighbourhood get them back ;)
        Cell.Inactive Me Cell.InactiveNothing ->
            4

        -- Should not happen
        Cell.Void ->
            0



-- Compute all possible trainings


compute : Map -> Dict ( Int, Int ) Cell -> List Training
compute map frontier =
    Dict.foldl (helper map) [] frontier


helper : Map -> ( Int, Int ) -> Cell -> List Training -> List Training
helper map ( x, y ) cell acc =
    -- This should be precomputed from the list of buildings instead
    if Map.isProtectedByEnemyTower x y map then
        Training 3 x y cell :: acc

    else
        case cell of
            -- On all other cells, we might want to train
            Cell.Neutral ->
                Training 1 x y cell :: acc

            Cell.Active Enemy Cell.ActiveNothing ->
                Training 1 x y cell :: acc

            Cell.Active Enemy (Cell.ActiveBuilding building) ->
                case building.type_ of
                    Tower ->
                        Training 3 x y cell :: acc

                    _ ->
                        Training 1 x y cell :: acc

            Cell.Active Enemy (Cell.ActiveUnit unit) ->
                case unit.level of
                    1 ->
                        Training 2 x y cell :: acc

                    _ ->
                        Training 3 x y cell :: acc

            Cell.Inactive Enemy Cell.InactiveNothing ->
                Training 1 x y cell :: acc

            Cell.Inactive Enemy (Cell.InactiveBuilding building) ->
                case building.type_ of
                    Tower ->
                        Training 3 x y cell :: acc

                    _ ->
                        Training 1 x y cell :: acc

            -- We cannot train on void cells
            -- We should not train on our active cells
            -- There should not be inactive friendly cell
            _ ->
                acc
