module Process.Dijkstra exposing (CostMap, Node, allCosts, toString)

import Array exposing (Array)
import Data.Cell as Cell exposing (Cell)
import Data.Map as Map exposing (Map)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Node =
    ( Int, Int )


type alias CostMap =
    Array (Array Int)


update : Int -> Int -> Int -> CostMap -> CostMap
update x y cost costMap =
    case Array.get y costMap of
        Nothing ->
            costMap

        Just line ->
            Array.set y (Array.set x cost line) costMap



-- Convert to string for visualization


toString : CostMap -> String
toString costMap =
    Array.map lineString costMap
        |> Array.toList
        |> String.join "\n"


lineString : Array Int -> String
lineString line =
    Array.map costString line
        |> Array.toList
        |> String.join " , "


costString : Int -> String
costString n =
    if n >= 0 && n < 10 then
        String.cons ' ' (String.fromInt n)

    else
        String.fromInt n



-- Compute all costs


allCosts : Node -> Map -> CostMap
allCosts node map =
    Array.repeat 12 (Array.repeat 12 -1)
        |> costHelper (Dict.singleton node 0) Set.empty map



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
                update x y cost costMap

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
