port module Main exposing (main)

import Array exposing (Array)
import Decode
import Game
import Json.Decode as Decode exposing (Decoder, Value)


main : Program Value Game.State Value
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
init : Value -> ( Game.State, Cmd msg )
init data =
    case Decode.decodeValue Decode.initData data of
        Ok minesSpots ->
            ( Game.State minesSpots, debug "Init Done!" )

        Err error ->
            ( Game.State Array.empty, debug (Decode.errorToString error) )


{-| Function called during game loop with game data of current turn.
Extract the data with a Json decoder.
-}
update : Value -> Game.State -> ( Game.State, Cmd msg )
update data state =
    case Decode.decodeValue Decode.gameData data of
        Ok gameData ->
            let
                ( newState, theOrders, log ) =
                    Game.strategy gameData state
            in
            -- ( state, Cmd.batch [ order theOrders ] )
            ( newState, Cmd.batch [ debug log, order theOrders ] )

        Err error ->
            ( state, debug (Decode.errorToString error) )
