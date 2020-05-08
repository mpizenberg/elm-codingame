module Game exposing (Data, Pacman, Pellet, State, defaultState, strategy)


type alias State =
    { width : Int
    , height : Int
    , rows : List String
    }


defaultState : State
defaultState =
    { width = 0
    , height = 0
    , rows = []
    }


type alias Data =
    { turn : Int
    , myScore : Int
    , opponentScore : Int
    , visiblePacCount : Int
    , pacs : List Pacman
    , visiblePelletCount : Int
    , pellets : List Pellet
    }


type alias Pacman =
    { pacId : Int
    , mine : Bool
    , x : Int
    , y : Int
    , typeId : String
    , speedTurnsLeft : Int
    , abilityCooldown : Int
    }


type alias Pellet =
    { x : Int
    , y : Int
    , value : Int
    }


strategy : Data -> State -> ( State, String, Maybe String )
strategy data state =
    ( state, "MOVE 0 15 10", Nothing )
