module Game exposing (Data, InitData, PacmanData, PelletData, State, defaultState, init, nodeIds, strategy)

import Graph exposing (Graph)



-- Initialization


type alias InitData =
    { width : Int
    , height : Int
    , rows : List String
    }


type alias State =
    Graph Cell ()


type Cell
    = Empty
    | Wall
    | Pellet Int
    | Me Int
    | Opponent Int


defaultState : State
defaultState =
    Graph.empty


nodeIds : State -> List Int
nodeIds state =
    List.map .id (Graph.nodes state)


init : InitData -> ( State, Maybe String )
init { width, height, rows } =
    let
        { allNodes, allEdges } =
            List.foldl (accumRows width) (RowAccum 0 (List.repeat width Wall) [] []) rows

        graph =
            Graph.fromNodesAndEdges allNodes allEdges

        logNodes =
            Graph.nodes graph
                |> List.map (.id >> String.fromInt)
                |> String.join " "

        logEdges =
            Graph.edges graph
                |> List.filter (\e -> e.from < e.to)
                |> List.map (\e -> String.fromInt e.from ++ "-" ++ String.fromInt e.to)
                |> String.join " "
    in
    ( graph, Just (logNodes ++ "\n" ++ logEdges) )


type alias Node =
    { id : Int
    , label : Cell
    }


type alias Edge =
    { from : Int
    , to : Int
    , label : ()
    }


type alias RowAccum =
    { y : Int, topRow : List Cell, allNodes : List Node, allEdges : List Edge }


accumRows : Int -> String -> RowAccum -> RowAccum
accumRows width row { y, topRow, allNodes, allEdges } =
    let
        thisRow =
            cellsFromString row

        update x leftCell cell nodesAndEdges =
            let
                leftId =
                    subToId width (modBy width (x - 1)) y

                id =
                    subToId width x y
            in
            case ( x, leftCell, cell ) of
                -- Passage between left and right borders
                ( 0, _, Empty ) ->
                    { allNodes = Node id cell :: nodesAndEdges.allNodes
                    , allEdges = Edge leftId id () :: Edge id leftId () :: nodesAndEdges.allEdges
                    }

                -- New node with and edge with the left node
                ( _, Empty, Empty ) ->
                    { allNodes = Node id cell :: nodesAndEdges.allNodes
                    , allEdges = Edge leftId id () :: Edge id leftId () :: nodesAndEdges.allEdges
                    }

                -- New node without edge
                ( _, _, Empty ) ->
                    { allNodes = Node id cell :: nodesAndEdges.allNodes
                    , allEdges = nodesAndEdges.allEdges
                    }

                -- Otherwise no new node
                _ ->
                    nodesAndEdges

        accumNodesAndEdges cell ( x, leftCell, nodesAndEdges ) =
            ( x + 1, cell, update x leftCell cell nodesAndEdges )

        ( _, _, updated ) =
            List.foldl accumNodesAndEdges ( 0, Wall, { allNodes = allNodes, allEdges = allEdges } ) thisRow

        topEdges =
            List.map2 (\top this -> top == Empty && this == Empty) topRow thisRow
                |> List.indexedMap (buildTopEdges width y)
                |> List.concat
    in
    { y = y + 1, topRow = thisRow, allNodes = updated.allNodes, allEdges = reversePrepend topEdges updated.allEdges }


reversePrepend : List a -> List a -> List a
reversePrepend l1 l2 =
    case l1 of
        [] ->
            l2

        l :: ls ->
            reversePrepend ls (l :: l2)


buildTopEdges : Int -> Int -> Int -> Bool -> List Edge
buildTopEdges width y x hasEdge =
    if hasEdge then
        let
            topId =
                subToId width x (y - 1)

            id =
                subToId width x y
        in
        [ Edge topId id (), Edge id topId () ]

    else
        []


cellsFromString : String -> List Cell
cellsFromString str =
    String.foldr (\char cells -> cellFromChar char :: cells) [] str


cellFromChar : Char -> Cell
cellFromChar char =
    case char of
        ' ' ->
            Empty

        _ ->
            Wall


subToId : Int -> Int -> Int -> Int
subToId width x y =
    width * y + x



-- Game turn


type alias Data =
    { turn : Int
    , myScore : Int
    , opponentScore : Int
    , visiblePacCount : Int
    , pacs : List PacmanData
    , visiblePelletCount : Int
    , pellets : List PelletData
    }


type alias PacmanData =
    { pacId : Int
    , mine : Bool
    , x : Int
    , y : Int
    , typeId : String
    , speedTurnsLeft : Int
    , abilityCooldown : Int
    }


type alias PelletData =
    { x : Int
    , y : Int
    , value : Int
    }


strategy : Data -> State -> ( State, String, Maybe String )
strategy data state =
    ( state, "MOVE 0 15 10", Nothing )
