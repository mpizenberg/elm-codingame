module Data.Pacman exposing (Pacman)


type alias Pacman =
    { pacId : Int
    , mine : Bool
    , x : Int
    , y : Int
    , typeId : String
    , speedTurnsLeft : Int
    , abilityCooldown : Int
    }
