module Strategy.SuperPellets exposing (individual)

import Data.Pacman exposing (Pacman)
import Dict exposing (Dict)
import Random


subToInd : Int -> Int -> Int -> Int
subToInd width x y =
    width * y + x


individual : Dict Int (List Int) -> Int -> Int -> Int -> Int -> List Pacman -> Pacman -> ( Int, Maybe String )
individual pellets turn target width height oldPacs pac =
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
