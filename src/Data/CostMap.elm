module Data.CostMap exposing
    ( CostMap
    , get
    , toString
    , update
    )

import Array exposing (Array)


type alias CostMap =
    Array (Array Int)


get : Int -> Int -> CostMap -> Int
get x y costMap =
    case Array.get y costMap of
        Nothing ->
            -1

        Just line ->
            Maybe.withDefault -1 (Array.get x line)


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
