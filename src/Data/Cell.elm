module Data.Cell exposing
    ( Active(..)
    , Cell(..)
    , Inactive(..)
    , fromChar
    , toChar
    )

import Data.Shared exposing (..)


type Cell
    = Void
    | Neutral
    | Active Owner Active
    | Inactive Owner Inactive


type Active
    = ActiveNothing
    | ActiveBuilding Building
    | ActiveUnit Unit


type Inactive
    = InactiveNothing
    | InactiveBuilding Building


toChar : Cell -> Char
toChar cell =
    case cell of
        Void ->
            '#'

        Neutral ->
            '.'

        Active Me _ ->
            'O'

        Active Enemy _ ->
            'X'

        Inactive Me _ ->
            'o'

        Inactive Enemy _ ->
            'x'


fromChar : Char -> Result String Cell
fromChar char =
    case char of
        '#' ->
            Ok Void

        '.' ->
            Ok Neutral

        'O' ->
            Ok (Active Me ActiveNothing)

        'X' ->
            Ok (Active Enemy ActiveNothing)

        'o' ->
            Ok (Inactive Me InactiveNothing)

        'x' ->
            Ok (Inactive Enemy InactiveNothing)

        _ ->
            Err "Incorrect char"
