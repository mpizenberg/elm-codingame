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
    , isMine : Bool
    }


order : Training -> String
order { level, x, y } =
    String.join " " ("TRAIN" :: List.map String.fromInt [ level, x, y ])


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


compute : Map -> Dict ( Int, Int ) Cell -> List Training
compute map frontier =
    Dict.foldl (helper map) [] frontier


helper : Map -> ( Int, Int ) -> Cell -> List Training -> List Training
helper map ( x, y ) cell acc =
    case Map.get x y map of
        Just Cell.Neutral ->
            Training 1 x y False :: acc

        Just (Cell.Active Enemy Cell.ActiveNothing) ->
            Training 1 x y False :: acc

        Just (Cell.Active Enemy (Cell.ActiveBuilding building)) ->
            case building.type_ of
                Tower ->
                    acc

                _ ->
                    Training 1 x y True :: acc

        Just (Cell.Active Enemy (Cell.ActiveUnit unit)) ->
            case unit.level of
                1 ->
                    Training 2 x y False :: acc

                _ ->
                    acc

        Just (Cell.Inactive Enemy Cell.InactiveNothing) ->
            Training 1 x y False :: acc

        Just (Cell.Inactive _ (Cell.InactiveBuilding building)) ->
            case building.type_ of
                Tower ->
                    acc

                _ ->
                    Training 1 x y True :: acc

        _ ->
            acc
