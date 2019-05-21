port module Main exposing (main)

import Array exposing (Array)
import Decode
import Json.Decode as Decode exposing (Decoder, Value)
import Types exposing (..)


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
                log =
                    String.join "\n\n" <|
                        [ "Turn: " ++ String.fromInt gameData.turn

                        -- , "Terrain:\n" ++ terrainToString gameData.terrain
                        ]
            in
            ( model, Cmd.batch [ debug log, order "WAIT" ] )

        Err error ->
            ( model, debug (Decode.errorToString error) )



-- Types


type alias Model =
    { minesSpots : Array Position
    }
