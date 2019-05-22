module Data.Shared exposing
    ( Building
    , BuildingType(..)
    , Owner(..)
    , Position
    , Unit
    )


type alias Building =
    { owner : Owner
    , type_ : BuildingType
    , x : Int
    , y : Int
    }


type BuildingType
    = Hq
    | Mine
    | Tower


type alias Unit =
    { owner : Owner
    , id : Int
    , level : Int
    , x : Int
    , y : Int
    }


type Owner
    = Me
    | Enemy


type alias Position =
    { x : Int
    , y : Int
    }
