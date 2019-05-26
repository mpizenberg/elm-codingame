module Process.Dijkstra exposing (Step, allCosts, spear)

import Array exposing (Array)
import Data.Cell as Cell exposing (Cell)
import Data.CostMap as CostMap exposing (CostMap)
import Data.Map as Map exposing (Map)
import Data.Shared exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Node =
    ( Int, Int )



-- Compute all costs


allCosts : ( Int, Int ) -> Map -> CostMap
allCosts origin map =
    Array.repeat 12 (Array.repeat 12 -1)
        |> costHelper (Dict.singleton origin 0) Set.empty map



-- Process a node :
-- Pick the lower cost node in the nodes still to process.
-- Update it on cost map.
-- Add it to the set of already processed nodes.
-- For each neighbour:
--     if it is in the set of already processed nodes, do nothing
--     otherwise,
--         compute its access cost from this node (here just +1)
--         if it is lower than its already computed cost,
--              update it in nodes still to process
-- recurse


costHelper : Dict Node Int -> Set Node -> Map -> CostMap -> CostMap
costHelper toProcess processed map costMap =
    if Dict.isEmpty toProcess then
        costMap

    else
        let
            ( ( x, y ) as node, cost ) =
                lowestCost toProcess

            updatedToProcess =
                Dict.remove node toProcess

            newCostMap =
                CostMap.update x y cost costMap

            newProcessed =
                Set.insert node processed

            nodeNeighbours =
                neighbours node map

            toProcessNext =
                List.foldl (evalNeighbour processed cost) updatedToProcess nodeNeighbours
        in
        costHelper toProcessNext newProcessed map newCostMap


evalNeighbour : Set Node -> Int -> Node -> Dict Node Int -> Dict Node Int
evalNeighbour processed cost node toProcess =
    --     if it is in the set of already processed nodes, do nothing
    --     otherwise,
    --         compute its access cost from this node (here just +1)
    --         if it is lower than its already computed cost,
    --              update it in nodes still to process
    if Set.member node processed then
        toProcess

    else
        let
            nodeCost =
                cost + 1

            chooseMin maybeOldCost =
                case maybeOldCost of
                    Nothing ->
                        Just nodeCost

                    Just oldCost ->
                        Just (min oldCost nodeCost)
        in
        Dict.update node chooseMin toProcess


lowestCost : Dict Node Int -> ( Node, Int )
lowestCost =
    Dict.foldl
        (\node cost (( ln, lc ) as pair) ->
            if cost < lc then
                ( node, cost )

            else
                pair
        )
        ( ( -1, -1 ), 1000000000 )


neighbours : Node -> Map -> List Node
neighbours ( x, y ) map =
    List.filterMap identity
        [ accessNode x (y - 1) map
        , accessNode (x - 1) y map
        , accessNode (x + 1) y map
        , accessNode x (y + 1) map
        ]


accessNode : Int -> Int -> Map -> Maybe Node
accessNode x y map =
    case Map.get x y map of
        Nothing ->
            Nothing

        Just Cell.Void ->
            Nothing

        _ ->
            Just ( x, y )



-- Helpers


allNodes : List Node
allNodes =
    crossProduct range12 range12


crossProduct : List a -> List a -> List ( a, a )
crossProduct l1 l2 =
    List.concatMap (\x1 -> List.map (Tuple.pair x1) l2) l1


range12 : List Int
range12 =
    List.range 0 11



-- SPEAR TO HQ #################################################################


type alias Step =
    { x : Int
    , y : Int
    , level : Int
    }


spear : Int -> Node -> Map -> List Step
spear money (( x, y ) as target) map =
    let
        targetCost =
            Maybe.withDefault 0 (transitionCost x y map Set.empty)

        toProcess =
            Dict.singleton target ( targetCost, [ Step x y (targetCost // 10) ] )
    in
    spearHelper money toProcess Set.empty map


spearHelper : Int -> Dict Node ( Int, List Step ) -> Set Node -> Map -> List Step
spearHelper money toProcess processed map =
    if Dict.isEmpty toProcess then
        []

    else
        let
            ( ( x, y ) as node, ( cost, trainingPath ) as data ) =
                lowestTrainingCost toProcess
        in
        if cost > money then
            []

        else if Map.isActiveMe x y map then
            trainingPath

        else
            let
                updatedToProcess =
                    Dict.remove node toProcess

                newProcessed =
                    Set.insert node processed

                nodeNeighbours =
                    neighbours node map

                toProcessNext =
                    List.foldl (evalTransition processed data map) updatedToProcess nodeNeighbours
            in
            spearHelper money toProcessNext newProcessed map


lowestTrainingCost : Dict Node ( Int, List Step ) -> ( Node, ( Int, List Step ) )
lowestTrainingCost =
    Dict.foldl
        (\node ( cost, trainings ) (( ln, ( lc, lt ) ) as pair) ->
            if cost < lc then
                ( node, ( cost, trainings ) )

            else
                pair
        )
        ( ( -1, -1 ), ( 1000000000, [] ) )


evalTransition : Set Node -> ( Int, List Step ) -> Map -> Node -> Dict Node ( Int, List Step ) -> Dict Node ( Int, List Step )
evalTransition processed ( cost, trainingPath ) map (( x, y ) as node) toProcess =
    case transitionCost x y map processed of
        Nothing ->
            toProcess

        Just gold ->
            Dict.update node (improveTransition node gold cost trainingPath) toProcess


improveTransition : Node -> Int -> Int -> List Step -> Maybe ( Int, List Step ) -> Maybe ( Int, List Step )
improveTransition ( x, y ) gold newCost trainingPath maybeData =
    case maybeData of
        Nothing ->
            if gold == 0 then
                Just ( newCost, trainingPath )

            else
                Just ( newCost + gold, Step x y (gold // 10) :: trainingPath )

        (Just ( cost, _ )) as data ->
            let
                newTotalCost =
                    newCost + gold
            in
            if newTotalCost < cost && gold == 0 then
                Just ( newTotalCost, trainingPath )

            else if newTotalCost < cost then
                Just ( newTotalCost, Step x y (gold // 10) :: trainingPath )

            else
                data


transitionCost : Int -> Int -> Map -> Set Node -> Maybe Int
transitionCost x y map processed =
    if Set.member ( x, y ) processed then
        Nothing

    else
        case Map.get x y map of
            Nothing ->
                Nothing

            Just Cell.Void ->
                Nothing

            Just cell ->
                if Map.isProtectedByEnemyTower x y map then
                    Just 30

                else
                    case cell of
                        Cell.Active Enemy (Cell.ActiveBuilding building) ->
                            case building.type_ of
                                Tower ->
                                    Just 30

                                _ ->
                                    Just 10

                        Cell.Active Enemy (Cell.ActiveUnit unit) ->
                            case unit.level of
                                1 ->
                                    Just 20

                                _ ->
                                    Just 30

                        Cell.Inactive Enemy (Cell.InactiveBuilding building) ->
                            case building.type_ of
                                Tower ->
                                    Just 30

                                _ ->
                                    Just 10

                        Cell.Active Me _ ->
                            Just 0

                        Cell.Inactive Me _ ->
                            Just 0

                        _ ->
                            Just 10
