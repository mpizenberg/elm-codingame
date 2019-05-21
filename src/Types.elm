module Types exposing
    ( Building
    , BuildingType(..)
    , Cell(..)
    , GameData
    , Owner(..)
    , Position
    , Terrain
    , Unit
    , cellFromChar
    , cellsLineString
    , terrainToString
    )

import Array exposing (Array)


type alias GameData =
    { turn : Int
    , gold : Int
    , income : Int
    , enemyGold : Int
    , enemyIncome : Int
    , terrain : Terrain
    , buildings : Array Building
    , units : Array Unit
    }



-- Terrain


type alias Terrain =
    Array (Array Cell)


terrainToString : Terrain -> String
terrainToString terrain =
    Array.toList terrain
        |> List.map cellsLineString
        |> String.join "\n"


cellsLineString : Array Cell -> String
cellsLineString cells =
    Array.map cellToChar cells
        |> Array.toList
        |> String.fromList



-- Cell


type Cell
    = Void
    | Neutral
    | Active Owner
    | Inactive Owner


cellToChar : Cell -> Char
cellToChar cell =
    case cell of
        Void ->
            '#'

        Neutral ->
            '.'

        Active Me ->
            'O'

        Active Enemy ->
            'X'

        Inactive Me ->
            'o'

        Inactive Enemy ->
            'x'


cellFromChar : Char -> Result String Cell
cellFromChar char =
    case char of
        '#' ->
            Ok Void

        '.' ->
            Ok Neutral

        'O' ->
            Ok (Active Me)

        'X' ->
            Ok (Active Enemy)

        'o' ->
            Ok (Inactive Me)

        'x' ->
            Ok (Inactive Enemy)

        _ ->
            Err "Incorrect char"



-- Building


type alias Building =
    { owner : Owner
    , type_ : BuildingType
    , x : Int
    , y : Int
    }


type BuildingType
    = Hq
    | Mine
    | Tower



-- Unit


type alias Unit =
    { owner : Owner
    , id : Int
    , level : Int
    , x : Int
    , y : Int
    }



-- Other stuff


type Owner
    = Me
    | Enemy


type alias Position =
    { x : Int
    , y : Int
    }
