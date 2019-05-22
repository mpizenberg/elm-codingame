port module Main exposing (main)

import Array exposing (Array)
import Data.Cell as Cell exposing (Cell)
import Data.Map as Map exposing (Map)
import Data.Shared exposing (..)
import Decode
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Process.Frontier as Frontier
import Process.Movement as Movement
import Process.Training as Training


main : Program Value Model Value
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = \_ -> incoming identity
        }


{-| Port bringing the updated game data every turn.
-}
port incoming : (Value -> msg) -> Sub msg


{-| Port to give the new orders for this turn.
-}
port order : String -> Cmd msg


{-| Port to help debugging, will print using console.error().
-}
port debug : String -> Cmd msg


{-| Function called at initialization.
the Value contains initial game data.
Extract with a Json decoder.
-}
init : Value -> ( Model, Cmd msg )
init data =
    case Decode.decodeValue Decode.initData data of
        Ok minesSpots ->
            ( Model minesSpots, debug "Init Done!" )

        Err error ->
            ( Model Array.empty, debug (Decode.errorToString error) )


{-| Function called during game loop with game data of current turn.
Extract the data with a Json decoder.
-}
update : Value -> Model -> ( Model, Cmd msg )
update data model =
    case Decode.decodeValue Decode.gameData data of
        Ok gameData ->
            let
                enemyHqPos =
                    case Map.get 0 0 gameData.map of
                        Just (Cell.Active Me _) ->
                            { x = 11, y = 11 }

                        _ ->
                            { x = 0, y = 0 }

                ( myUnits, enemyUnits ) =
                    List.partition (\u -> u.owner == Me) gameData.units

                ( myBuildings, enemyBuildings ) =
                    List.partition (\b -> b.owner == Me) gameData.buildings

                updatedMapWithUnits =
                    List.foldl Map.updateUnit gameData.map gameData.units

                updatedMapWithBuildings =
                    List.foldl Map.updateBuilding updatedMapWithUnits gameData.buildings

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
                    Training.spend gameData.gold sortedTraining []

                theOrders =
                    String.join ";" <|
                        List.concat
                            [ List.map Movement.order sortedMovements
                            , List.map Training.order paidTraining
                            , [ "WAIT" ]
                            ]

                log =
                    String.join "\n\n" <|
                        [ "Turn: " ++ String.fromInt gameData.turn
                        , "Training:\n" ++ trainingString
                        , "TrainingComparable:\n" ++ trainingComparableString
                        , "Frontier:\n" ++ frontierPosString

                        -- , "Map:\n" ++ Map.toString newMap
                        ]
            in
            -- ( model, Cmd.batch [ order theOrders ] )
            ( model, Cmd.batch [ debug log, order theOrders ] )

        Err error ->
            ( model, debug (Decode.errorToString error) )



-- Types


type alias Model =
    { minesSpots : Array Pos
    }
