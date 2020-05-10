module Strategy.SuperPellets exposing (execute)

import Data.Pacman exposing (Pacman)
import Data.State exposing (State)
import Data.Turn as Turn
import Dict exposing (Dict)
import Random


subToInd : Int -> Int -> Int -> Int
subToInd width x y =
    width * y + x


indToSub : Int -> Int -> ( Int, Int )
indToSub width index =
    ( modBy width index, index // width )


execute : Turn.Data -> State -> ( State, List ( Int, Int, Int ), Maybe String )
execute data state =
    let
        ( pellets, superPellets ) =
            List.partition (\p -> p.value == 1) data.pellets

        dictPellets =
            if List.isEmpty pellets && List.isEmpty superPellets then
                Dict.empty

            else if List.isEmpty pellets then
                Dict.fromList [ ( 10, List.map (\p -> subToInd state.width p.x p.y) superPellets ) ]

            else if List.isEmpty superPellets then
                Dict.fromList [ ( 1, List.map (\p -> subToInd state.width p.x p.y) pellets ) ]

            else
                Dict.fromList
                    [ ( 1, List.map (\p -> subToInd state.width p.x p.y) pellets )
                    , ( 10, List.map (\p -> subToInd state.width p.x p.y) superPellets )
                    ]

        ( myPacs, enemyVisiblePacs ) =
            List.partition .mine data.pacs

        individualStrat target pac =
            if data.turn == 0 then
                individual dictPellets data.turn state.width state.height state.myPacs target pac

            else
                individual dictPellets data.turn state.width state.height state.myPacs target pac

        ( newTargets, listMaybeLogs ) =
            List.map2 individualStrat state.targets myPacs
                |> List.unzip

        positions =
            List.map (indToSub state.width) newTargets
                |> List.indexedMap (\i ( x, y ) -> ( i, x, y ))

        logs =
            case List.filterMap identity listMaybeLogs of
                [] ->
                    Nothing

                someLogs ->
                    Just (String.join "\n" someLogs)

        newState =
            { state | targets = newTargets, myPacs = myPacs }
    in
    ( newState, positions, logs )


individual : Dict Int (List Int) -> Int -> Int -> Int -> List Pacman -> Int -> Pacman -> ( Int, Maybe String )
individual pellets turn width height oldPacs target pac =
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
