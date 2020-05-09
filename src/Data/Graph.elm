module Data.Graph exposing (Graph, conv, empty, fromRows)

import Graph exposing (Node, NodeContext)
import IntDict


type alias Graph a =
    Graph.Graph a ()


type alias Edge =
    Graph.Edge ()



-- Initialization


empty : Graph a
empty =
    Graph.empty


mkEdge : Int -> Int -> Edge
mkEdge id1 id2 =
    Graph.Edge id1 id2 ()


type alias RowAccum a =
    { y : Int, topRow : List Bool, allNodes : List (Node a), allEdges : List Edge }


fromRows : a -> Int -> List String -> Graph a
fromRows default width rows =
    let
        { allNodes, allEdges } =
            List.foldl (accumRows default width) (RowAccum 0 (List.repeat width True) [] []) rows

        graph =
            Graph.fromNodesAndEdges allNodes allEdges

        -- logNodes =
        --     Graph.nodes graph
        --         |> List.map (.id >> String.fromInt)
        --         |> String.join " "
        --
        -- logEdges =
        --     Graph.edges graph
        --         |> List.filter (\e -> e.from < e.to)
        --         |> List.map (\e -> String.fromInt e.from ++ "-" ++ String.fromInt e.to)
        --         |> String.join " "
    in
    graph


accumRows : a -> Int -> String -> RowAccum a -> RowAccum a
accumRows default width row { y, topRow, allNodes, allEdges } =
    let
        thisRow =
            String.foldr (\char walls -> wallFromChar char :: walls) [] row

        update x leftCell cell nodesAndEdges =
            let
                leftId =
                    subToInd width (modBy width (x - 1)) y

                id =
                    subToInd width x y
            in
            case ( x, leftCell, cell ) of
                -- Passage between left and right borders
                ( 0, _, False ) ->
                    { allNodes = Node id default :: nodesAndEdges.allNodes
                    , allEdges = mkEdge leftId id :: mkEdge id leftId :: nodesAndEdges.allEdges
                    }

                -- New node with and edge with the left node
                ( _, False, False ) ->
                    { allNodes = Node id default :: nodesAndEdges.allNodes
                    , allEdges = mkEdge leftId id :: mkEdge id leftId :: nodesAndEdges.allEdges
                    }

                -- New node without edge
                ( _, _, False ) ->
                    { allNodes = Node id default :: nodesAndEdges.allNodes
                    , allEdges = nodesAndEdges.allEdges
                    }

                -- Otherwise no new node
                _ ->
                    nodesAndEdges

        accumNodesAndEdges cell ( x, leftCell, nodesAndEdges ) =
            ( x + 1, cell, update x leftCell cell nodesAndEdges )

        ( _, _, updated ) =
            List.foldl accumNodesAndEdges ( 0, True, { allNodes = allNodes, allEdges = allEdges } ) thisRow

        topEdges =
            List.map2 (\top this -> not top && not this) topRow thisRow
                |> List.indexedMap (buildTopEdges width y)
                |> List.concat
    in
    { y = y + 1, topRow = thisRow, allNodes = updated.allNodes, allEdges = reversePrepend topEdges updated.allEdges }


wallFromChar : Char -> Bool
wallFromChar char =
    case char of
        ' ' ->
            False

        _ ->
            True


buildTopEdges : Int -> Int -> Int -> Bool -> List Edge
buildTopEdges width y x hasEdge =
    if hasEdge then
        let
            topId =
                subToInd width x (y - 1)

            id =
                subToInd width x y
        in
        [ mkEdge topId id, mkEdge id topId ]

    else
        []


reversePrepend : List a -> List a -> List a
reversePrepend l1 l2 =
    case l1 of
        [] ->
            l2

        l :: ls ->
            reversePrepend ls (l :: l2)


subToInd : Int -> Int -> Int -> Int
subToInd width x y =
    width * y + x



-- Convolutions


type alias ConvKernel a =
    List a -> a -> a


conv : ConvKernel a -> Graph a -> Graph a
conv kernel graph =
    Graph.mapContexts (nodeConv kernel graph) graph


nodeConv : ConvKernel a -> Graph a -> NodeContext a () -> NodeContext a ()
nodeConv kernel origGraph ctx =
    let
        origNeighborLabels =
            IntDict.foldl (\id _ list -> Graph.get id origGraph :: list) [] ctx.incoming
                |> List.filterMap (Maybe.map (.node >> .label))

        newLabel =
            kernel origNeighborLabels ctx.node.label

        newNode =
            { id = ctx.node.id
            , label = newLabel
            }
    in
    { node = newNode
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }
