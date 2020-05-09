port module Main exposing (main)

import Data.State exposing (State)
import Data.Turn
import Game
import Graph
import Json.Decode exposing (Value)


main : Program Value State Value
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
init : Value -> ( State, Cmd msg )
init data =
    case Json.Decode.decodeValue Game.decodeInitData data of
        Ok initData ->
            case Game.init initData of
                ( state, Nothing ) ->
                    ( state, Cmd.none )

                ( state, Just log ) ->
                    ( state, debug log )

        Err error ->
            ( Data.State.default, debug (Json.Decode.errorToString error) )


{-| Function called during game loop with game data of current turn.
Extract the data with a Json decoder.
-}
update : Value -> State -> ( State, Cmd msg )
update data state =
    case Json.Decode.decodeValue Data.Turn.decode data of
        Ok gameData ->
            case Game.step gameData state of
                ( newState, theOrders, Nothing ) ->
                    ( newState, order theOrders )

                ( newState, theOrders, Just log ) ->
                    ( newState, Cmd.batch [ order theOrders, debug log ] )

        Err error ->
            ( state, debug (Json.Decode.errorToString error) )
