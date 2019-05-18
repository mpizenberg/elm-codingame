port module Main exposing (main)

import Json.Encode exposing (Value)


main : Program Value () Value
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
init : Value -> ( (), Cmd msg )
init _ =
    ( (), debug "Init Done!" )


{-| Function called during game loop with game data of current turn.
Extract the data with a Json decoder.
-}
update : Value -> () -> ( (), Cmd msg )
update _ _ =
    ( (), Cmd.batch [ debug "Update Done!", order "WAIT" ] )
