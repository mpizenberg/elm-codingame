module Types exposing
    ( ActiveCell(..)
    , Building
    , BuildingType(..)
    , Cell(..)
    , GameData
    , InactiveCell(..)
    , Owner(..)
    , Position
    , Terrain
    , Training
    , Unit
    , cellFromChar
    , cellsLineString
    , getCell
    , isActiveMe
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


isActiveMe : Int -> Int -> Terrain -> Bool
isActiveMe x y terrain =
    case getCell x y terrain of
        Just (Active Me _) ->
            True

        _ ->
            False


getCell : Int -> Int -> Terrain -> Maybe Cell
getCell x y terrain =
    Array.get y terrain
        |> Maybe.andThen (Array.get x)


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
    | Active Owner ActiveCell
    | Inactive Owner InactiveCell


type ActiveCell
    = ActiveUnknown
    | ActiveBuilding Building
    | ActiveUnit Unit


type InactiveCell
    = InactiveUnknown
    | InactiveBuilding Building


cellToChar : Cell -> Char
cellToChar cell =
    case cell of
        Void ->
            '#'

        Neutral ->
            '.'

        Active Me _ ->
            'O'

        Active Enemy _ ->
            'X'

        Inactive Me _ ->
            'o'

        Inactive Enemy _ ->
            'x'


cellFromChar : Char -> Result String Cell
cellFromChar char =
    case char of
        '#' ->
            Ok Void

        '.' ->
            Ok Neutral

        'O' ->
            Ok (Active Me ActiveUnknown)

        'X' ->
            Ok (Active Enemy ActiveUnknown)

        'o' ->
            Ok (Inactive Me InactiveUnknown)

        'x' ->
            Ok (Inactive Enemy InactiveUnknown)

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



-- Training


type alias Training =
    { level : Int
    , x : Int
    , y : Int
    , isMine : Bool
    }



-- Other stuff


type Owner
    = Me
    | Enemy


type alias Position =
    { x : Int
    , y : Int
    }
