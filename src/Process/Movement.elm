module Process.Movement exposing
    ( Capture
    , Movement
    , compute
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
    List.foldl (helper enemyHqPos) ( map, [] ) (sort enemyHqPos units)



-- Movements can prevent other units to move.
-- Ideally, we should find an optimal list of compatible movements.
-- In practice it is simpler to use a heuristic.
-- For example, move first units in front.
--
-- We could also move first units that have less options.


sort : Pos -> List Unit -> List Unit
sort enemyHqPos units =
    let
        distance unit =
            abs (unit.x - enemyHqPos.x) + abs (unit.y - enemyHqPos.y)
    in
    List.sortBy distance units



-- Movements are performed one after the over.
-- It is thus important to keep track of our units on the Map after each move.


helper : Pos -> Unit -> ( Map, List Movement ) -> ( Map, List Movement )
helper enemyHqPos unit ( map, movAcc ) =
    let
        noMovement =
            Movement unit.id unit.x unit.y NoCapture

        possibleMovements =
            noMovement
                :: List.filterMap identity
                    [ canMove unit unit.x (unit.y - 1) map
                    , canMove unit (unit.x - 1) unit.y map
                    , canMove unit (unit.x + 1) unit.y map
                    , canMove unit unit.x (unit.y + 1) map
                    ]

        sortedMovements =
            List.sortBy (comparable enemyHqPos) possibleMovements

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
    if Map.isProtectedByEnemyTower x y map then
        Nothing

    else
        case Map.get x y map of
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


comparable : Pos -> Movement -> ( Int, Int )
comparable { x, y } m =
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
