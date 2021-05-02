module CodinGame exposing (Ports, Runtime, worker)

import Json.Decode exposing (Value)


type alias Runtime state =
    { init : Value -> ( state, Maybe String )
    , turn : Value -> state -> ( state, String, Maybe String )
    }


type alias Ports =
    { incoming : Sub Value
    , command : String -> Cmd Value
    , debug : String -> Cmd Value
    }


worker : Ports -> Runtime state -> Program Value state Value
worker ports { init, turn } =
    Platform.worker
        { init = setup ports init
        , update = update ports turn
        , subscriptions = \_ -> ports.incoming
        }


setup : Ports -> (Value -> ( state, Maybe String )) -> Value -> ( state, Cmd Value )
setup ports init flags =
    let
        ( state, maybeDebug ) =
            init flags
    in
    case maybeDebug of
        Nothing ->
            ( state, Cmd.none )

        Just debug ->
            ( state, ports.debug debug )


update : Ports -> (Value -> state -> ( state, String, Maybe String )) -> Value -> state -> ( state, Cmd Value )
update ports turn msg state =
    let
        ( newState, command, maybeDebug ) =
            turn msg state
    in
    case maybeDebug of
        Nothing ->
            ( newState, ports.command command )

        Just debug ->
            ( newState, Cmd.batch [ ports.command command, ports.debug debug ] )
