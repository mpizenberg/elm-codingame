module Compute exposing (affordableTraining, movements, myFrontierCells, sortTraining, training)

import Array exposing (Array)
import Dict exposing (Dict)
import Types exposing (..)



-- Movements


movements : Position -> Terrain -> List Unit -> ( Terrain, List Movement )
movements enemyHqPos terrain units =
    let
        distance unit =
            abs (unit.x - enemyHqPos.x) + abs (unit.y - enemyHqPos.y)

        sortedUnits =
            List.sortBy distance units
    in
    List.foldl (movementsHelper enemyHqPos) ( terrain, [] ) sortedUnits


movementsHelper : Position -> Unit -> ( Terrain, List Movement ) -> ( Terrain, List Movement )
movementsHelper enemyHqPos unit ( terrain, movAcc ) =
    let
        noMovement =
            Movement unit.id unit.x unit.y NoCapture

        possibleMovements =
            List.filterMap identity
                [ Just noMovement
                , canMove unit unit.x (unit.y - 1) terrain
                , canMove unit (unit.x - 1) unit.y terrain
                , canMove unit (unit.x + 1) unit.y terrain
                , canMove unit unit.x (unit.y + 1) terrain
                ]

        sortedMovements =
            List.sortBy (movementComparable enemyHqPos) possibleMovements

        chosenMovement =
            Maybe.withDefault noMovement (List.head sortedMovements)

        newMovAcc =
            if chosenMovement.x == unit.x && chosenMovement.y == unit.y then
                movAcc

            else
                chosenMovement :: movAcc

        newTerrain =
            if chosenMovement.x == unit.x && chosenMovement.y == unit.y then
                terrain

            else
                updateTerrain chosenMovement.x chosenMovement.y (Active Me (ActiveUnit unit)) terrain
                    |> updateTerrain unit.x unit.y (Active Me ActiveNothing)
    in
    ( newTerrain, newMovAcc )


canMove : Unit -> Int -> Int -> Terrain -> Maybe Movement
canMove unit x y terrain =
    if isEnemyProtected x y terrain then
        Nothing

    else
        case getCell x y terrain of
            Just Neutral ->
                Just (Movement unit.id x y CaptureNeutral)

            Just (Active Me ActiveNothing) ->
                Just (Movement unit.id x y NoCapture)

            Just (Active Enemy ActiveNothing) ->
                Just (Movement unit.id x y CaptureEnemy)

            Just (Inactive Me InactiveNothing) ->
                Just (Movement unit.id x y NoCapture)

            Just (Inactive Enemy InactiveNothing) ->
                Just (Movement unit.id x y CaptureEnemy)

            Just (Active Me (ActiveBuilding building)) ->
                case building.type_ of
                    Tower ->
                        Nothing

                    _ ->
                        Just (Movement unit.id x y NoCapture)

            Just (Active Enemy (ActiveBuilding building)) ->
                case building.type_ of
                    Tower ->
                        Nothing

                    _ ->
                        Just (Movement unit.id x y CaptureEnemy)

            Just (Active Enemy (ActiveUnit enemyUnit)) ->
                case ( enemyUnit.level, unit.level ) of
                    ( _, 3 ) ->
                        Just (Movement unit.id x y CaptureEnemy)

                    ( 1, 2 ) ->
                        Just (Movement unit.id x y CaptureEnemy)

                    _ ->
                        Nothing

            Just (Inactive Enemy (InactiveBuilding building)) ->
                case building.type_ of
                    Tower ->
                        Nothing

                    _ ->
                        Just (Movement unit.id x y CaptureEnemy)

            _ ->
                Nothing


isEnemyProtected : Int -> Int -> Terrain -> Bool
isEnemyProtected x y terrain =
    case getCell x y terrain of
        Just (Active Me _) ->
            False

        Just (Inactive Me _) ->
            False

        _ ->
            isEnemyTower x (y - 1) terrain
                || isEnemyTower (x - 1) y terrain
                || isEnemyTower (x + 1) y terrain
                || isEnemyTower x (y + 1) terrain


isEnemyTower : Int -> Int -> Terrain -> Bool
isEnemyTower x y terrain =
    case getCell x y terrain of
        Just (Active Enemy (ActiveBuilding building)) ->
            building.type_ == Tower

        _ ->
            False


movementComparable : Position -> Movement -> ( Int, Int )
movementComparable { x, y } m =
    let
        captureScore =
            case m.capture of
                CaptureEnemy ->
                    0

                CaptureNeutral ->
                    1

                NoCapture ->
                    2

        -- TODO
        -- Should be better to go toward enemy territory
        distance =
            abs (x - m.x) + abs (y - m.y)
    in
    ( captureScore, distance )



-- Training


affordableTraining : Int -> List Training -> List Training -> List Training
affordableTraining gold list acc =
    case list of
        [] ->
            acc

        x :: xs ->
            let
                newGold =
                    gold - 10 * x.level
            in
            if newGold >= 0 then
                affordableTraining newGold xs (x :: acc)

            else
                acc


sortTraining : Position -> List Training -> List Training
sortTraining pos =
    List.sortBy (trainingComparable pos)


trainingComparable : Position -> Training -> ( Int, Int )
trainingComparable { x, y } t =
    let
        distance =
            abs (x - t.x) + abs (y - t.y)
    in
    ( distance, t.level )


training : Terrain -> Dict ( Int, Int ) Cell -> List Training
training terrain frontier =
    Dict.foldl (trainingHelper terrain) [] frontier


trainingHelper : Terrain -> ( Int, Int ) -> Cell -> List Training -> List Training
trainingHelper terrain ( x, y ) cell acc =
    case getCell x y terrain of
        Just Neutral ->
            Training 1 x y False :: acc

        Just (Active Enemy ActiveNothing) ->
            Training 1 x y False :: acc

        Just (Active Enemy (ActiveBuilding building)) ->
            case building.type_ of
                Tower ->
                    acc

                _ ->
                    Training 1 x y True :: acc

        Just (Active Enemy (ActiveUnit unit)) ->
            case unit.level of
                1 ->
                    Training 2 x y False :: acc

                _ ->
                    acc

        Just (Inactive Enemy InactiveNothing) ->
            Training 1 x y False :: acc

        Just (Inactive _ (InactiveBuilding building)) ->
            case building.type_ of
                Tower ->
                    acc

                _ ->
                    Training 1 x y True :: acc

        _ ->
            acc



-- Frontier


myFrontierCells : Terrain -> Dict ( Int, Int ) Cell
myFrontierCells terrain =
    helperFrontier terrain 0 0 Dict.empty


helperFrontier : Terrain -> Int -> Int -> Dict ( Int, Int ) Cell -> Dict ( Int, Int ) Cell
helperFrontier terrain x y cells =
    case ( x, y ) of
        ( _, 12 ) ->
            cells

        ( 12, _ ) ->
            helperFrontier terrain 0 (y + 1) cells

        _ ->
            case isAtMyFrontier x y terrain of
                Just cell ->
                    helperFrontier terrain (x + 1) y (Dict.insert ( x, y ) cell cells)

                Nothing ->
                    helperFrontier terrain (x + 1) y cells


isAtMyFrontier : Int -> Int -> Terrain -> Maybe Cell
isAtMyFrontier x y terrain =
    case getCell x y terrain of
        Nothing ->
            Nothing

        Just Void ->
            Nothing

        Just (Active Me _) ->
            Nothing

        Just cell ->
            if hasActiveFriendlyNeighbour x y terrain then
                Just cell

            else
                Nothing


hasActiveFriendlyNeighbour : Int -> Int -> Terrain -> Bool
hasActiveFriendlyNeighbour x y terrain =
    isActiveMe (x - 1) y terrain
        || isActiveMe (x + 1) y terrain
        || isActiveMe x (y - 1) terrain
        || isActiveMe x (y + 1) terrain
