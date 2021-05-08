port module Main exposing (main)

import CodinGame
import Json.Decode as Decode exposing (Decoder, Value)


{-| Port bringing the updated game data every turn.
-}
port stdin : (Value -> msg) -> Sub msg


{-| Port to give the new commands for this turn.
-}
port stdout : String -> Cmd msg


{-| Port to help debugging, will print using console.error().
-}
port stderr : String -> Cmd msg


main : Program Value GameState Value
main =
    CodinGame.worker
        { stdin = stdin identity
        , stdout = stdout
        , stderr = stderr
        }
        { init = init
        , turn = turn
        }


type alias GameState =
    { turnCount : Int
    , nodeCount : Int
    , linkCount : Int
    , exitCount : Int
    , links : List ( Int, Int )
    , exits : List Int
    }


defaultGameState : GameState
defaultGameState =
    { turnCount = 0
    , nodeCount = 0
    , linkCount = 0
    , exitCount = 0
    , links = []
    , exits = []
    }


type alias InitData =
    { nodeCount : Int
    , linkCount : Int
    , exitCount : Int
    , links : List ( Int, Int )
    , exits : List Int
    }


type alias TurnData =
    { skynetNode : Int
    }


initDataDecoder : Decoder InitData
initDataDecoder =
    Decode.map5 InitData
        (Decode.field "nodeCount" Decode.int)
        (Decode.field "linkCount" Decode.int)
        (Decode.field "exitCount" Decode.int)
        (Decode.field "links" (Decode.list (pairDecoder Decode.int Decode.int)))
        (Decode.field "exits" (Decode.list Decode.int))


pairDecoder : Decoder a -> Decoder b -> Decoder ( a, b )
pairDecoder decA decB =
    Decode.map2 Tuple.pair
        (Decode.index 0 decA)
        (Decode.index 1 decB)


turnDataDecoder : Decoder TurnData
turnDataDecoder =
    Decode.map TurnData
        (Decode.field "skynetNode" Decode.int)


{-| Function called at the game initialization.
The string contains initial game data.
-}
init : Value -> ( GameState, Maybe String )
init value =
    case Decode.decodeValue initDataDecoder value of
        Err err ->
            -- Should not happen once we've written the decoder correctly
            ( defaultGameState, Just (Decode.errorToString err) )

        Ok initData ->
            ( { turnCount = 0
              , nodeCount = initData.nodeCount
              , linkCount = initData.linkCount
              , exitCount = initData.exitCount
              , links = initData.links
              , exits = initData.exits
              }
            , Just "Initialization done!"
            )


{-| Function called during the game loop with the data of the current turn.
-}
turn : Value -> GameState -> ( GameState, String, Maybe String )
turn value state =
    case Decode.decodeValue turnDataDecoder value of
        Err err ->
            -- Should not happen once we've written the decoder correctly
            ( { state | turnCount = state.turnCount + 1 }
            , ""
            , Just (Decode.errorToString err)
            )

        Ok turnData ->
            ( { state | turnCount = state.turnCount + 1 }
            , "1 2"
            , Nothing
            )
