port module Main exposing (main)

import Array exposing (Array)
import Json.Decode as Decode exposing (Decoder, Value)


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
    case Decode.decodeValue initDataDecoder data of
        Ok minesSpots ->
            ( Model minesSpots, debug "Init Done!" )

        Err error ->
            ( Model Array.empty, debug (Decode.errorToString error) )


{-| Function called during game loop with game data of current turn.
Extract the data with a Json decoder.
-}
update : Value -> Model -> ( Model, Cmd msg )
update data model =
    case Decode.decodeValue gameDataDecoder data of
        Ok gameData ->
            ( model, Cmd.batch [ debug (String.fromInt gameData.turn), order "WAIT" ] )

        Err error ->
            ( model, debug (Decode.errorToString error) )



-- Types


type alias Model =
    { minesSpots : Array Position
    }


type alias Position =
    { x : Int
    , y : Int
    }


type alias GameData =
    { turn : Int
    , gold : Int
    , income : Int
    , opponentGold : Int
    , opponentIncome : Int
    , terrain : ()
    , buildings : ()
    , units : ()
    }



-- Decoders


initDataDecoder : Decoder (Array Position)
initDataDecoder =
    Decode.field "mines" <|
        Decode.map Array.fromList <|
            Decode.list positionDecoder


positionDecoder : Decoder Position
positionDecoder =
    Decode.map2 Position
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)


gameDataDecoder : Decoder GameData
gameDataDecoder =
    Decode.map8 GameData
        (Decode.field "turn" Decode.int)
        (Decode.field "gold" Decode.int)
        (Decode.field "income" Decode.int)
        (Decode.field "opponentGold" Decode.int)
        (Decode.field "opponentIncome" Decode.int)
        (Decode.field "terrain" (Decode.succeed ()))
        (Decode.field "buildings" (Decode.succeed ()))
        (Decode.field "units" (Decode.succeed ()))
