module Process.Training exposing
    ( Training
    , comparable
    , compute
    , order
    , sort
    , spend
    )

import Data.Cell as Cell exposing (Cell)
import Data.Shared exposing (..)
import Data.Terrain as Terrain exposing (Terrain)
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


sort : Position -> Terrain -> List Training -> List Training
sort pos terrain =
    List.sortBy (comparable pos terrain)


comparable : Position -> Terrain -> Training -> ( Int, Int, Int )
comparable { x, y } terrain t =
    let
        distance =
            abs (x - t.x) + abs (y - t.y)

        nbFriendlyNeighbourUnits =
            List.length <|
                List.filter identity
                    [ isMyUnit t.x (t.y - 1) terrain
                    , isMyUnit (t.x - 1) t.y terrain
                    , isMyUnit (t.x + 1) t.y terrain
                    , isMyUnit t.x (t.y + 1) terrain
                    ]
    in
    ( nbFriendlyNeighbourUnits, distance, t.level )


isMyUnit : Int -> Int -> Terrain -> Bool
isMyUnit x y terrain =
    case Terrain.getCell x y terrain of
        Just (Cell.Active Me (Cell.ActiveUnit _)) ->
            True

        _ ->
            False


compute : Terrain -> Dict ( Int, Int ) Cell -> List Training
compute terrain frontier =
    Dict.foldl (trainingHelper terrain) [] frontier


trainingHelper : Terrain -> ( Int, Int ) -> Cell -> List Training -> List Training
trainingHelper terrain ( x, y ) cell acc =
    case Terrain.getCell x y terrain of
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
