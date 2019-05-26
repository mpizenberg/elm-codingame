module Process.Movement exposing
    ( Capture
    , Movement
    , compute
    , order
    )

import Data.Cell as Cell exposing (Cell)
import Data.CostMap as CostMap exposing (CostMap)
import Data.Map as Map exposing (Map)
import Data.Shared exposing (..)


type alias Movement =
    { id : Int
    , x : Int
    , y : Int
    , cell : Cell
    }


type Capture
    = NoCapture
    | CaptureNeutral
    | CaptureEnemy


order : Movement -> String
order { id, x, y } =
    String.join " " ("MOVE" :: List.map String.fromInt [ id, x, y ])


compute : CostMap -> CostMap -> Map -> List Unit -> ( Map, List Movement )
compute costMap criticalMap map units =
    List.foldl (helper costMap criticalMap) ( map, [] ) (sort costMap units)



-- Movements can prevent other units to move.
-- Ideally, we should find an optimal list of compatible movements.
-- In practice it is simpler to use a heuristic.
-- For example, move first units in front.
--
-- We could also move first units that have less options.


sort : CostMap -> List Unit -> List Unit
sort costMap units =
    let
        distance unit =
            CostMap.get unit.x unit.y costMap
    in
    List.sortBy distance units



-- Movements are performed one after the other.
-- It is thus important to keep track of our units on the Map after each move.


helper : CostMap -> CostMap -> Unit -> ( Map, List Movement ) -> ( Map, List Movement )
helper costMap criticalMap unit ( map, movAcc ) =
    let
        unitCell =
            Cell.Active Me (Cell.ActiveUnit unit)

        noMovement =
            Movement unit.id unit.x unit.y unitCell

        possibleMovements =
            noMovement
                :: List.filterMap identity
                    [ canMove unit unit.x (unit.y - 1) map
                    , canMove unit (unit.x - 1) unit.y map
                    , canMove unit (unit.x + 1) unit.y map
                    , canMove unit unit.x (unit.y + 1) map
                    ]

        sortedMovements =
            List.sortBy (score costMap criticalMap map) possibleMovements

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
    let
        maybeCell =
            Map.get x y map
    in
    if Map.isProtectedByEnemyTower x y map then
        case ( unit.level, maybeCell ) of
            ( 3, Just Cell.Void ) ->
                Nothing

            ( 3, Just cell ) ->
                Just (Movement unit.id x y cell)

            _ ->
                Nothing

    else
        case maybeCell of
            Nothing ->
                Nothing

            Just cell ->
                case cell of
                    -- My building
                    Cell.Active Me (Cell.ActiveBuilding building) ->
                        case building.type_ of
                            Mine ->
                                Just (Movement unit.id x y cell)

                            _ ->
                                Nothing

                    Cell.Inactive Me (Cell.InactiveBuilding building) ->
                        case building.type_ of
                            Mine ->
                                Just (Movement unit.id x y cell)

                            _ ->
                                Nothing

                    -- Enemy building
                    Cell.Inactive Enemy (Cell.InactiveBuilding building) ->
                        case ( building.type_, unit.level ) of
                            ( Tower, 1 ) ->
                                Nothing

                            ( Tower, 2 ) ->
                                Nothing

                            _ ->
                                Just (Movement unit.id x y cell)

                    Cell.Active Enemy (Cell.ActiveBuilding building) ->
                        case ( building.type_, unit.level ) of
                            ( Tower, 1 ) ->
                                Nothing

                            ( Tower, 2 ) ->
                                Nothing

                            _ ->
                                Just (Movement unit.id x y cell)

                    -- Enemy unit
                    Cell.Active Enemy (Cell.ActiveUnit enemyUnit) ->
                        case ( enemyUnit.level, unit.level ) of
                            ( _, 3 ) ->
                                Just (Movement unit.id x y cell)

                            ( 1, 2 ) ->
                                Just (Movement unit.id x y cell)

                            _ ->
                                Nothing

                    -- My unit
                    Cell.Active Me (Cell.ActiveUnit _) ->
                        Nothing

                    -- Void cannot
                    Cell.Void ->
                        Nothing

                    -- Neutral and empty can
                    _ ->
                        Just (Movement unit.id x y cell)


score : CostMap -> CostMap -> Map -> Movement -> Int
score costMap criticalMap map m =
    let
        targetScore =
            baseScore m.cell

        d1Score =
            List.sum (List.map baseScore (Map.getDistance1Cells m.x m.y map))

        d2Score =
            List.sum (List.map baseScore (Map.getDistance2Cells m.x m.y map))

        distance =
            CostMap.get m.x m.y costMap

        critical =
            2 * CostMap.get m.x m.y criticalMap

        spread =
            2 * abs (m.x - m.y)
    in
    -- Negate because of increase sort order
    -(8 * targetScore + 2 * d1Score + d2Score - distance + critical - spread)


baseScore : Cell -> Int
baseScore cell =
    case cell of
        Cell.Neutral ->
            1

        Cell.Inactive Enemy Cell.InactiveNothing ->
            2

        Cell.Active Enemy Cell.ActiveNothing ->
            2

        Cell.Active Enemy (Cell.ActiveUnit _) ->
            2

        Cell.Inactive _ (Cell.InactiveBuilding building) ->
            case building.type_ of
                Tower ->
                    4

                Mine ->
                    5

                Hq ->
                    10

        Cell.Active Enemy (Cell.ActiveBuilding building) ->
            case building.type_ of
                Tower ->
                    5

                Mine ->
                    6

                Hq ->
                    10

        -- We want negative score on our Active units (not too crowded for expansion).
        Cell.Active Me (Cell.ActiveUnit _) ->
            -1

        -- Actually if we are in risky area (confrontation), we should stay near friendly cells.
        -- So maybe we could have a mode.
        --     - greedy -> -1
        --     - defensive -> +1
        Cell.Active Me _ ->
            -1

        -- If we have Inactive cells in neighbourhood get them back ;)
        Cell.Inactive Me Cell.InactiveNothing ->
            4

        -- Should not happen
        Cell.Void ->
            0
