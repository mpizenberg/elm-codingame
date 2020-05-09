module Game exposing (Data, InitData, PacmanData, PelletData, State, defaultState, init, strategy)

import Dict exposing (Dict)
import Graph exposing (Graph)
import Random



-- Initialization


type alias InitData =
    { width : Int
    , height : Int
    , rows : List String
    }


type alias State =
    { width : Int
    , height : Int
    , graph : Graph Cell ()
    , targets : List Int
    , myPacs : List PacmanData
    }


type Cell
    = Empty
    | Pellet Int
    | Me Int
    | Opponent Int


defaultState : State
defaultState =
    { width = 0, height = 0, graph = Graph.empty, targets = [], myPacs = [] }


init : InitData -> ( State, Maybe String )
init { width, height, rows } =
    let
        { allNodes, allEdges } =
            List.foldl (accumRows width) (RowAccum 0 (List.repeat width True) [] []) rows

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
    ( { width = width, height = height, graph = graph, targets = [], myPacs = [] }
    , Just (logNodes ++ "\n" ++ logEdges)
    )


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
    { y : Int, topRow : List Bool, allNodes : List Node, allEdges : List Edge }


accumRows : Int -> String -> RowAccum -> RowAccum
accumRows width row { y, topRow, allNodes, allEdges } =
    let
        thisRow =
            wallsFromString row

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
                    { allNodes = Node id Empty :: nodesAndEdges.allNodes
                    , allEdges = Edge leftId id () :: Edge id leftId () :: nodesAndEdges.allEdges
                    }

                -- New node with and edge with the left node
                ( _, False, False ) ->
                    { allNodes = Node id Empty :: nodesAndEdges.allNodes
                    , allEdges = Edge leftId id () :: Edge id leftId () :: nodesAndEdges.allEdges
                    }

                -- New node without edge
                ( _, _, False ) ->
                    { allNodes = Node id Empty :: nodesAndEdges.allNodes
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
                subToInd width x (y - 1)

            id =
                subToInd width x y
        in
        [ Edge topId id (), Edge id topId () ]

    else
        []


wallsFromString : String -> List Bool
wallsFromString str =
    String.foldr (\char walls -> wallFromChar char :: walls) [] str


wallFromChar : Char -> Bool
wallFromChar char =
    case char of
        ' ' ->
            False

        _ ->
            True


subToInd : Int -> Int -> Int -> Int
subToInd width x y =
    width * y + x


indToSub : Int -> Int -> ( Int, Int )
indToSub width id =
    ( modBy width id, id // width )



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
    let
        graphWithPacmans =
            List.foldl (placePacmans state.width) state.graph data.pacs

        graphWithPellets =
            List.foldl (placePellets state.width) graphWithPacmans data.pellets

        incrementPellets p dict =
            case Dict.get p.value dict of
                Nothing ->
                    Dict.insert p.value [ subToInd state.width p.x p.y ] dict

                Just ids ->
                    Dict.insert p.value (subToInd state.width p.x p.y :: ids) dict

        pellets =
            List.foldl incrementPellets Dict.empty data.pellets

        myPacs =
            List.filter .mine data.pacs
                |> List.sortBy .pacId

        pacStrat target pac =
            pacStratValue pellets data.turn target state.width state.height state.myPacs pac

        ( newTargets, log ) =
            List.unzip <|
                case ( data.turn, myPacs, state.targets ) of
                    ( 0, _, _ ) ->
                        List.map (pacStrat 0) myPacs

                    ( _, _, _ ) ->
                        List.map2 pacStrat state.targets myPacs

        toStringOrder { pacId } t =
            let
                ( x, y ) =
                    indToSub state.width t
            in
            [ pacId, x, y ]
                |> List.map String.fromInt
                |> (::) "MOVE"
                |> String.join " "

        stringOrders =
            List.map2 toStringOrder myPacs newTargets
                |> String.join " | "
    in
    ( { state | targets = newTargets, myPacs = myPacs }
    , stringOrders
    , Just <| String.join "\n" (List.filterMap identity log)
    )


placePacmans : Int -> PacmanData -> Graph Cell () -> Graph Cell ()
placePacmans width { pacId, mine, x, y } graph =
    -- TODO
    graph


placePellets : Int -> PelletData -> Graph Cell () -> Graph Cell ()
placePellets width { x, y, value } graph =
    -- TODO
    graph



-- Individual Pacmans strategies


{-| Targets the pellet with most value
-}
pacStratValue : Dict Int (List Int) -> Int -> Int -> Int -> Int -> List PacmanData -> PacmanData -> ( Int, Maybe String )
pacStratValue pellets turn target width height oldPacs pac =
    let
        pos =
            subToInd width pac.x pac.y

        oldPac =
            List.head (List.drop pac.pacId oldPacs)
                |> Maybe.withDefault pac

        oldPos =
            subToInd width oldPac.x oldPac.y

        bigValue =
            Dict.keys pellets
                |> List.sortBy (\v -> -v)
                |> List.head
                |> Maybe.withDefault 0

        bigTargets =
            Dict.get bigValue pellets
                |> Maybe.withDefault []

        targetInBig =
            List.member target bigTargets

        nbBig =
            List.length bigTargets

        maxDrop =
            nbBig - 1

        seed =
            Random.initialSeed (turn + pos)

        ( randDrop, _ ) =
            Random.step (Random.int 0 maxDrop) seed

        ( randEverywhere, _ ) =
            Random.step (Random.int 0 (width * height - 1)) seed
    in
    if Dict.isEmpty pellets then
        if oldPos == pos then
            ( randEverywhere, Just "toto" )

        else
            ( target, Just "tata" )

    else if targetInBig then
        -- change target if we are blocked
        if oldPos == pos then
            if nbBig == 1 then
                ( randEverywhere, Just "hoho" )

            else
                ( Maybe.withDefault 0 (List.head <| List.drop randDrop bigTargets), Just "1" )

        else
            ( target, Just "2" )

    else if bigValue == 1 then
        ( Maybe.withDefault 0 (List.head <| List.drop randDrop bigTargets), Just "3" )

    else
        ( Maybe.withDefault 0 (List.head <| List.drop (min maxDrop oldPac.pacId) bigTargets), Just (Debug.toString pellets) )
