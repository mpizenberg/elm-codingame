module Data.State exposing (Cell(..), State, default, init)

import Data.Graph as Graph exposing (Graph)
import Data.Pacman exposing (Pacman)
import Data.Turn as Turn exposing (Pellet)


type alias State =
    { width : Int
    , height : Int
    , targets : List Int
    , myPacs : List Pacman
    , graph : Graph Cell
    }


type Cell
    = Empty
    | Pellet Int
    | Me Pacman
    | Opponent Pacman


default : State
default =
    { width = 0, height = 0, graph = Graph.empty, targets = [], myPacs = [] }


init : { width : Int, height : Int, rows : List String } -> State
init { width, height, rows } =
    { width = width
    , height = height
    , graph = Graph.fromRows Empty width rows
    , targets = []
    , myPacs = []
    }



-- Turn update


update : Turn.Data -> State -> State
update data state =
    state


placePacmans : Int -> Pacman -> Graph Cell -> Graph Cell
placePacmans width { pacId, mine, x, y } graph =
    -- TODO
    graph


placePellets : Int -> Pellet -> Graph Cell -> Graph Cell
placePellets width { x, y, value } graph =
    -- TODO
    graph
