module Process.Movement exposing
    ( Capture(..)
    , Movement
    , canMove
    , compute
    , isEnemyProtected
    , isEnemyTower
    , movementComparable
    , movementsHelper
    , order
    )

import Data.Cell as Cell exposing (Cell)
import Data.Shared exposing (..)
import Data.Terrain as Terrain exposing (Terrain)


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


order : Movement -> String
order { id, x, y } =
    String.join " " ("MOVE" :: List.map String.fromInt [ id, x, y ])


compute : Position -> Terrain -> List Unit -> ( Terrain, List Movement )
compute enemyHqPos terrain units =
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
                Terrain.update chosenMovement.x chosenMovement.y (Cell.Active Me (Cell.ActiveUnit unit)) terrain
                    |> Terrain.update unit.x unit.y (Cell.Active Me Cell.ActiveNothing)
    in
    ( newTerrain, newMovAcc )


canMove : Unit -> Int -> Int -> Terrain -> Maybe Movement
canMove unit x y terrain =
    if isEnemyProtected x y terrain then
        Nothing

    else
        case Terrain.getCell x y terrain of
            Just Cell.Neutral ->
                Just (Movement unit.id x y CaptureNeutral)

            Just (Cell.Active Me Cell.ActiveNothing) ->
                Just (Movement unit.id x y NoCapture)

            Just (Cell.Active Enemy Cell.ActiveNothing) ->
                Just (Movement unit.id x y CaptureEnemy)

            Just (Cell.Inactive Me Cell.InactiveNothing) ->
                Just (Movement unit.id x y NoCapture)

            Just (Cell.Inactive Enemy Cell.InactiveNothing) ->
                Just (Movement unit.id x y CaptureEnemy)

            Just (Cell.Active Me (Cell.ActiveBuilding building)) ->
                case building.type_ of
                    Tower ->
                        Nothing

                    _ ->
                        Just (Movement unit.id x y NoCapture)

            Just (Cell.Active Enemy (Cell.ActiveBuilding building)) ->
                case building.type_ of
                    Tower ->
                        Nothing

                    _ ->
                        Just (Movement unit.id x y CaptureEnemy)

            Just (Cell.Active Enemy (Cell.ActiveUnit enemyUnit)) ->
                case ( enemyUnit.level, unit.level ) of
                    ( _, 3 ) ->
                        Just (Movement unit.id x y CaptureEnemy)

                    ( 1, 2 ) ->
                        Just (Movement unit.id x y CaptureEnemy)

                    _ ->
                        Nothing

            Just (Cell.Inactive Enemy (Cell.InactiveBuilding building)) ->
                case building.type_ of
                    Tower ->
                        Nothing

                    _ ->
                        Just (Movement unit.id x y CaptureEnemy)

            _ ->
                Nothing


isEnemyProtected : Int -> Int -> Terrain -> Bool
isEnemyProtected x y terrain =
    case Terrain.getCell x y terrain of
        Just (Cell.Active Me _) ->
            False

        Just (Cell.Inactive Me _) ->
            False

        _ ->
            isEnemyTower x (y - 1) terrain
                || isEnemyTower (x - 1) y terrain
                || isEnemyTower (x + 1) y terrain
                || isEnemyTower x (y + 1) terrain


isEnemyTower : Int -> Int -> Terrain -> Bool
isEnemyTower x y terrain =
    case Terrain.getCell x y terrain of
        Just (Cell.Active Enemy (Cell.ActiveBuilding building)) ->
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
