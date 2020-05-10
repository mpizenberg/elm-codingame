module Game exposing (InitData, decodeInitData, init, step)

import Data.State as State exposing (State)
import Data.Turn as Turn
import Json.Decode as Decode exposing (Decoder)
import Strategy.SuperPellets as Strategy



-- Initialization


type alias InitData =
    { width : Int
    , height : Int
    , rows : List String
    }


decodeInitData : Decoder InitData
decodeInitData =
    Decode.map3 InitData
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
        (Decode.field "rows" (Decode.list Decode.string))


init : InitData -> ( State, Maybe String )
init data =
    ( State.init data
    , Nothing
    )



-- Game turn


step : Turn.Data -> State -> ( State, String, Maybe String )
step data state =
    let
        ( newState, targets, log ) =
            Strategy.execute data state
    in
    ( newState
    , String.join " | " (List.map stringOrder targets)
    , log
    )


stringOrder : ( Int, Int, Int ) -> String
stringOrder ( pacId, x, y ) =
    [ "MOVE", String.fromInt pacId, String.fromInt x, String.fromInt y ]
        |> String.join " "
