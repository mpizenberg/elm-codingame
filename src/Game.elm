module Game exposing (Data, State, strategy)

import Array exposing (Array)
import Data.Cell as Cell
import Data.Map as Map exposing (Map)
import Data.Shared exposing (..)
import Dict exposing (Dict)
import Process.Frontier as Frontier
import Process.Movement as Movement
import Process.Training as Training


type alias State =
    { minesSpots : Array Pos
    }


type alias Data =
    { turn : Int
    , gold : Int
    , income : Int
    , enemyGold : Int
    , enemyIncome : Int
    , map : Map
    , buildings : List Building
    , units : List Unit
    }


strategy : Data -> State -> ( State, String, String )
strategy data state =
    let
        enemyHqPos =
            case Map.get 0 0 data.map of
                Just (Cell.Active Me _) ->
                    { x = 11, y = 11 }

                _ ->
                    { x = 0, y = 0 }

        ( myUnits, enemyUnits ) =
            List.partition (\u -> u.owner == Me) data.units

        ( myBuildings, enemyBuildings ) =
            List.partition (\b -> b.owner == Me) data.buildings

        updatedMapWithUnits =
            List.foldl Map.updateUnit data.map data.units

        updatedMapWithBuildings =
            List.foldl Map.updateBuilding updatedMapWithUnits data.buildings

        ( newMap, movements ) =
            Movement.compute enemyHqPos updatedMapWithBuildings myUnits

        sortedMovements =
            List.reverse movements

        frontier =
            Frontier.compute newMap

        frontierPosString =
            Dict.keys frontier
                |> List.map (\( x, y ) -> String.fromInt x ++ ", " ++ String.fromInt y)
                |> String.join "\n"

        training =
            Training.compute newMap frontier

        sortedTraining =
            Training.sort enemyHqPos newMap training

        trainingString =
            List.map (\t -> List.map String.fromInt [ t.level, t.x, t.y ]) training
                |> List.map (String.join " ")
                |> String.join "\n"

        trainingComparableString =
            List.map (Training.comparable enemyHqPos newMap) training
                |> List.map (\( a, b, c ) -> [ String.fromInt a, String.fromInt b, String.fromInt c ])
                |> List.map (String.join " ")
                |> String.join "\n"

        paidTraining =
            Training.spend data.gold sortedTraining []

        theOrders =
            String.join ";" <|
                List.concat
                    [ List.map Movement.order sortedMovements
                    , List.map Training.order paidTraining
                    , [ "WAIT" ]
                    ]

        log =
            String.join "\n\n" <|
                [ "Turn: " ++ String.fromInt data.turn
                , "Training:\n" ++ trainingString
                , "TrainingComparable:\n" ++ trainingComparableString
                , "Frontier:\n" ++ frontierPosString

                -- , "Map:\n" ++ Map.toString newMap
                ]
    in
    ( state, theOrders, log )
