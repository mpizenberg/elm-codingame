module CodinGame exposing (Ports, Runtime, worker)

import Json.Decode exposing (Value)


type alias Runtime state =
    { init : Value -> ( state, Maybe String )
    , turn : Value -> state -> ( state, String, Maybe String )
    }


type alias Ports =
    { stdin : Sub Value
    , stdout : String -> Cmd Value
    , stderr : String -> Cmd Value
    }


worker : Ports -> Runtime state -> Program Value state Value
worker ports { init, turn } =
    Platform.worker
        { init = setup ports init
        , update = update ports turn
        , subscriptions = \_ -> ports.stdin
        }


setup : Ports -> (Value -> ( state, Maybe String )) -> Value -> ( state, Cmd Value )
setup ports init flags =
    let
        ( state, maybeStderr ) =
            init flags
    in
    case maybeStderr of
        Nothing ->
            ( state, Cmd.none )

        Just stderr ->
            ( state, ports.stderr stderr )


update : Ports -> (Value -> state -> ( state, String, Maybe String )) -> Value -> state -> ( state, Cmd Value )
update ports turn msg state =
    let
        ( newState, stdout, maybeStderr ) =
            turn msg state
    in
    case maybeStderr of
        Nothing ->
            ( newState, ports.stdout stdout )

        Just stderr ->
            ( newState, Cmd.batch [ ports.stdout stdout, ports.stderr stderr ] )
