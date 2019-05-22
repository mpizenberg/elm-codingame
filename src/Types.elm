module Types exposing
    ( ActiveCell(..)
    , Building
    , BuildingType(..)
    , Capture(..)
    , Cell(..)
    , GameData
    , InactiveCell(..)
    , Movement
    , Owner(..)
    , Position
    , Terrain
    , Training
    , Unit
    , cellFromChar
    , cellsLineString
    , getCell
    , isActiveMe
    , movementOrder
    , terrainToString
    , trainingOrder
    , updateBuildingOnTerrain
    , updateTerrain
    , updateUnitOnTerrain
    )

import Array exposing (Array)


type alias GameData =
    { turn : Int
    , gold : Int
    , income : Int
    , enemyGold : Int
    , enemyIncome : Int
    , terrain : Terrain
    , buildings : List Building
    , units : List Unit
    }



-- Terrain


type alias Terrain =
    Array (Array Cell)


updateUnitOnTerrain : Unit -> Terrain -> Terrain
updateUnitOnTerrain u =
    updateTerrain u.x u.y (Active u.owner (ActiveUnit u))


updateBuildingOnTerrain : Building -> Terrain -> Terrain
updateBuildingOnTerrain b terrain =
    case getCell b.x b.y terrain of
        Just (Active _ _) ->
            updateTerrain b.x b.y (Active b.owner (ActiveBuilding b)) terrain

        _ ->
            updateTerrain b.x b.y (Inactive b.owner (InactiveBuilding b)) terrain


updateTerrain : Int -> Int -> Cell -> Terrain -> Terrain
updateTerrain x y cell terrain =
    case Array.get y terrain of
        Nothing ->
            terrain

        Just line ->
            Array.set y (Array.set x cell line) terrain


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
    = ActiveNothing
    | ActiveBuilding Building
    | ActiveUnit Unit


type InactiveCell
    = InactiveNothing
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
            Ok (Active Me ActiveNothing)

        'X' ->
            Ok (Active Enemy ActiveNothing)

        'o' ->
            Ok (Inactive Me InactiveNothing)

        'x' ->
            Ok (Inactive Enemy InactiveNothing)

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


trainingOrder : Training -> String
trainingOrder { level, x, y } =
    String.join " " ("TRAIN" :: List.map String.fromInt [ level, x, y ])



-- Movement


type alias Movement =
    { id : Int
    , x : Int
    , y : Int
    , capture : Capture
    }


type Capture
    = NoCapture
    | CaptureNeutral
    | CaptureEnemy


movementOrder : Movement -> String
movementOrder { id, x, y } =
    String.join " " ("MOVE" :: List.map String.fromInt [ id, x, y ])



-- Other stuff


type Owner
    = Me
    | Enemy


type alias Position =
    { x : Int
    , y : Int
    }
