module Data.Graph exposing (Graph, conv)

import Graph
import IntDict


type alias Graph a =
    Graph.Graph a ()


{-| The pair should be interpreted as ( original value, conved value )
-}
type alias ConvKernel a =
    List a -> a -> a


conv : ConvKernel a -> Graph a -> Graph a
conv kernel graph =
    Graph.mapContexts (nodeConv kernel graph) graph


nodeConv : ConvKernel a -> Graph a -> Graph.NodeContext a () -> Graph.NodeContext a ()
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
