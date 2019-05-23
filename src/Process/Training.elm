module Process.Training exposing
    ( Training
    , comparable
    , compute
    , order
    , sort
    , spend
    )

import Data.Cell as Cell exposing (Cell)
import Data.Map as Map exposing (Map)
import Data.Shared exposing (..)
import Dict exposing (Dict)


type alias Training =
    { level : Int
    , x : Int
    , y : Int
    , cell : Cell
    }


order : Training -> String
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


sort : Pos -> Map -> List Training -> List Training
sort pos map =
    List.sortBy (comparable pos map)


comparable : Pos -> Map -> Training -> ( Int, Int, Int )
comparable { x, y } map t =
    let
        distance =
            abs (x - t.x) + abs (y - t.y)

        nbFriendlyNeighbourUnits =
            List.length <|
                List.filter identity
                    [ Map.isMyUnit t.x (t.y - 1) map
                    , Map.isMyUnit (t.x - 1) t.y map
                    , Map.isMyUnit (t.x + 1) t.y map
                    , Map.isMyUnit t.x (t.y + 1) map
                    ]
    in
    ( nbFriendlyNeighbourUnits, distance, t.level )



-- Compute all possible trainings


compute : Map -> Dict ( Int, Int ) Cell -> List Training
compute map frontier =
    Dict.foldl (helper map) [] frontier


helper : Map -> ( Int, Int ) -> Cell -> List Training -> List Training
helper map ( x, y ) cell acc =
    case cell of
        Cell.Neutral ->
            Training 1 x y cell :: acc

        Cell.Active Enemy Cell.ActiveNothing ->
            Training 1 x y cell :: acc

        Cell.Active Enemy (Cell.ActiveBuilding building) ->
            case building.type_ of
                Tower ->
                    acc

                _ ->
                    Training 1 x y cell :: acc

        Cell.Active Enemy (Cell.ActiveUnit unit) ->
            case unit.level of
                1 ->
                    Training 2 x y cell :: acc

                _ ->
                    acc

        Cell.Inactive Enemy Cell.InactiveNothing ->
            Training 1 x y cell :: acc

        Cell.Inactive _ (Cell.InactiveBuilding building) ->
            case building.type_ of
                Tower ->
                    acc

                _ ->
                    Training 1 x y cell :: acc

        _ ->
            acc
