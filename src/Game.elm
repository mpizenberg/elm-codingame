module Game exposing (Data, State, initialState, strategy)

import Array
import Data.Cell as Cell
import Data.CostMap as CostMap exposing (CostMap)
import Data.Map as Map exposing (Map)
import Data.Shared exposing (..)
import Dict exposing (Dict)
import Process.Dijkstra as Dijkstra
import Process.Frontier as Frontier
import Process.Movement as Movement exposing (Movement)
import Process.Training as Training exposing (Training)


type alias State =
    { minesSpots : List Pos
    , costMap : CostMap
    , criticalMap : CostMap
    }


initialState : List Pos -> State
initialState minesSpots =
    { minesSpots = minesSpots
    , costMap = Array.empty
    , criticalMap = Array.empty
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

        costMap =
            if data.turn == 0 then
                Dijkstra.allCosts ( enemyHqPos.x, enemyHqPos.y ) data.map

            else
                state.costMap

        criticalMap =
            if data.turn == 0 then
                Dijkstra.criticalMap data.map

            else
                state.criticalMap

        newState =
            if data.turn == 0 then
                { state | costMap = costMap, criticalMap = criticalMap }

            else
                state

        -- Update the Map with the positions of units and buildings
        updatedMapWithUnits =
            List.foldl Map.updateUnit data.map data.units

        updatedMapWithBuildings =
            List.foldl Map.updateBuilding updatedMapWithUnits data.buildings

        -- Compute my units movements
        myUnits =
            List.filter (\u -> u.owner == Me) data.units

        ( newMap, movements ) =
            Movement.compute costMap updatedMapWithBuildings myUnits

        sortedMovements =
            List.reverse movements

        -- Evaluate spearToHq
        spear =
            Dijkstra.spear data.gold ( enemyHqPos.x, enemyHqPos.y ) newMap
    in
    if List.isEmpty spear then
        let
            -- Compute my trainings
            frontier =
                Frontier.compute newMap

            training =
                Training.compute newMap frontier

            sortedTraining =
                Training.sort costMap newMap training

            paidTraining =
                Training.spend data.gold sortedTraining []

            -- Build the orders from movements and training
            theOrders =
                String.join ";" <|
                    List.concat
                        [ List.map Movement.order sortedMovements
                        , List.map Training.order paidTraining
                        , [ "WAIT" ]
                        ]

            -- Some log info
            trainingString =
                List.map (\t -> List.map String.fromInt [ t.level, t.x, t.y ]) training
                    |> List.map (String.join " ")
                    |> String.join "\n"

            frontierPosString =
                Dict.keys frontier
                    |> List.map (\( x, y ) -> String.fromInt x ++ ", " ++ String.fromInt y)
                    |> String.join "\n"

            log =
                String.join "\n\n" <|
                    [ "Turn: " ++ String.fromInt data.turn

                    -- , "Training:\n" ++ trainingString
                    -- , "Frontier:\n" ++ frontierPosString
                    -- , "Cost map:\n" ++ CostMap.toString costMap
                    , "Critical map:\n" ++ CostMap.toString criticalMap

                    -- , "Map:\n" ++ Map.toString newMap
                    ]
        in
        ( newState, theOrders, log )

    else
        let
            theOrders =
                String.join ";" <|
                    List.concat
                        [ List.map Movement.order sortedMovements
                        , List.map Training.order spear
                        , [ "MSG Speared!" ]
                        ]

            spearString =
                List.map (\{ x, y } -> String.fromInt x ++ ", " ++ String.fromInt y) spear
                    |> String.join "\n"

            log =
                String.join "\n\n" <|
                    [ "Turn: " ++ String.fromInt data.turn
                    , "Spear: " ++ spearString
                    ]
        in
        ( newState, theOrders, log )
