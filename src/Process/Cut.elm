module Process.Cut exposing (WeakPoint, cutPoint, pointToString, toString, weakEnemyPoints)

import Data.Cell as Cell exposing (Cell)
import Data.CostMap as CostMap exposing (CostMap)
import Data.Map as Map exposing (Map)
import Data.Shared exposing (..)
import Dict exposing (Dict)


type alias WeakPoint =
    { x : Int
    , y : Int
    , cell : Cell
    , neighbours : ( ( Int, Int, Cell ), ( Int, Int, Cell ) )
    }


type alias Cut =
    { p1 : ( Int, Int, Cell )
    , p2 : ( Int, Int, Cell )
    , rest : Dict Node Cell
    }


pointToString : WeakPoint -> String
pointToString { x, y } =
    String.fromInt x ++ " " ++ String.fromInt y


toString : Cut -> String
toString { p2 } =
    let
        ( x, y, _ ) =
            p2
    in
    String.fromInt x ++ " " ++ String.fromInt y


cutPoint : Node -> CostMap -> Map -> WeakPoint -> Maybe Cut
cutPoint enemyHq costMap map weakPoint =
    let
        ( ( x1, y1, c1 ) as w1, ( x2, y2, c2 ) as w2 ) =
            weakPoint.neighbours

        cost1 =
            CostMap.get x1 y1 costMap

        cost2 =
            CostMap.get x2 y2 costMap

        cutMap =
            Map.update weakPoint.x weakPoint.y Cell.Neutral map
    in
    if cost1 < cost2 then
        let
            rest =
                cutSet cutMap ( x2, y2 ) c2 ( x1, y1 ) enemyHq
        in
        if Dict.isEmpty rest then
            Nothing

        else
            Just (Cut w1 ( weakPoint.x, weakPoint.y, weakPoint.cell ) rest)

    else
        let
            rest =
                cutSet cutMap ( x1, y1 ) c1 ( x2, y2 ) enemyHq
        in
        if Dict.isEmpty rest then
            Nothing

        else
            Just (Cut w2 ( weakPoint.x, weakPoint.y, weakPoint.cell ) rest)


cutSet : Map -> Node -> Cell -> Node -> Node -> Dict Node Cell
cutSet map node1 cell node2 enemyHq =
    -- We stop if we encounter node2 or enemyHq
    expandUntil node2 enemyHq map (Dict.singleton node1 ( 0, cell )) Dict.empty


expandUntil : Node -> Node -> Map -> Dict Node ( Int, Cell ) -> Dict Node Cell -> Dict Node Cell
expandUntil node2 enemyHq map toProcess processed =
    if Dict.isEmpty toProcess then
        processed

    else
        let
            ( ( x, y ) as node, ( cost, cell ) as data ) =
                nearestNode toProcess
        in
        if node == node2 || node == enemyHq then
            Dict.empty

        else
            let
                updatedToProcess =
                    Dict.remove node toProcess

                newProcessed =
                    Dict.insert node cell processed

                nodeNeighbours =
                    List.filterMap identity
                        [ maybeActiveEnemy x (y - 1) map
                        , maybeActiveEnemy (x - 1) y map
                        , maybeActiveEnemy (x + 1) y map
                        , maybeActiveEnemy x (y + 1) map
                        ]

                toProcessNext =
                    List.foldl (evalTransition (cost + 1)) updatedToProcess nodeNeighbours
            in
            expandUntil node2 enemyHq map toProcessNext newProcessed


evalTransition : Int -> ( Int, Int, Cell ) -> Dict Node ( Int, Cell ) -> Dict Node ( Int, Cell )
evalTransition cost ( x, y, cell ) toProcess =
    Dict.update ( x, y ) (improveTransition cost cell) toProcess


improveTransition : Int -> Cell -> Maybe ( Int, Cell ) -> Maybe ( Int, Cell )
improveTransition cost cell maybeData =
    case maybeData of
        Nothing ->
            Just ( cost, cell )

        Just ( oldCost, _ ) ->
            if cost < oldCost then
                Just ( cost, cell )

            else
                maybeData


nearestNode : Dict Node ( Int, Cell ) -> ( Node, ( Int, Cell ) )
nearestNode =
    Dict.foldl
        (\node ( cost, cell ) (( ln, ( lc, lcell ) ) as pair) ->
            if cost < lc then
                ( node, ( cost, cell ) )

            else
                pair
        )
        ( ( -1, -1 ), ( 1000000000, Cell.Void ) )



-- Helper functions


weakEnemyPoints : Map -> List WeakPoint
weakEnemyPoints map =
    Map.foldl (pick map) [] map


pick : Map -> Int -> Int -> Cell -> List WeakPoint -> List WeakPoint
pick map x y cell points =
    case cell of
        Cell.Active Enemy _ ->
            case hasTwoActiveNeighbours x y map of
                Just neighbours ->
                    WeakPoint x y cell neighbours :: points

                Nothing ->
                    points

        _ ->
            points


hasTwoActiveNeighbours : Int -> Int -> Map -> Maybe ( ( Int, Int, Cell ), ( Int, Int, Cell ) )
hasTwoActiveNeighbours x y map =
    let
        neighbours =
            List.filterMap identity
                [ maybeActiveEnemy x (y - 1) map
                , maybeActiveEnemy (x - 1) y map
                , maybeActiveEnemy (x + 1) y map
                , maybeActiveEnemy x (y + 1) map
                ]
    in
    case neighbours of
        n1 :: n2 :: [] ->
            Just ( n1, n2 )

        _ ->
            Nothing


maybeActiveEnemy : Int -> Int -> Map -> Maybe ( Int, Int, Cell )
maybeActiveEnemy x y map =
    case Map.get x y map of
        Just ((Cell.Active Enemy _) as cell) ->
            Just ( x, y, cell )

        _ ->
            Nothing
