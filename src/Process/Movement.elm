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
import Data.Map as Map exposing (Map)
import Data.Shared exposing (..)


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


compute : Pos -> Map -> List Unit -> ( Map, List Movement )
compute enemyHqPos map units =
    let
        distance unit =
            abs (unit.x - enemyHqPos.x) + abs (unit.y - enemyHqPos.y)

        sortedUnits =
            List.sortBy distance units
    in
    List.foldl (movementsHelper enemyHqPos) ( map, [] ) sortedUnits


movementsHelper : Pos -> Unit -> ( Map, List Movement ) -> ( Map, List Movement )
movementsHelper enemyHqPos unit ( map, movAcc ) =
    let
        noMovement =
            Movement unit.id unit.x unit.y NoCapture

        possibleMovements =
            List.filterMap identity
                [ Just noMovement
                , canMove unit unit.x (unit.y - 1) map
                , canMove unit (unit.x - 1) unit.y map
                , canMove unit (unit.x + 1) unit.y map
                , canMove unit unit.x (unit.y + 1) map
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

        newMap =
            if chosenMovement.x == unit.x && chosenMovement.y == unit.y then
                map

            else
                Map.update chosenMovement.x chosenMovement.y (Cell.Active Me (Cell.ActiveUnit unit)) map
                    |> Map.update unit.x unit.y (Cell.Active Me Cell.ActiveNothing)
    in
    ( newMap, newMovAcc )


canMove : Unit -> Int -> Int -> Map -> Maybe Movement
canMove unit x y map =
    if isEnemyProtected x y map then
        Nothing

    else
        case Map.getCell x y map of
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


isEnemyProtected : Int -> Int -> Map -> Bool
isEnemyProtected x y map =
    case Map.getCell x y map of
        Just (Cell.Active Me _) ->
            False

        Just (Cell.Inactive Me _) ->
            False

        _ ->
            isEnemyTower x (y - 1) map
                || isEnemyTower (x - 1) y map
                || isEnemyTower (x + 1) y map
                || isEnemyTower x (y + 1) map


isEnemyTower : Int -> Int -> Map -> Bool
isEnemyTower x y map =
    case Map.getCell x y map of
        Just (Cell.Active Enemy (Cell.ActiveBuilding building)) ->
            building.type_ == Tower

        _ ->
            False


movementComparable : Pos -> Movement -> ( Int, Int )
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
