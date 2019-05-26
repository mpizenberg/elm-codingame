module Data.Shared exposing
    ( Building
    , BuildingType(..)
    , Node
    , Owner(..)
    , Pos
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


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Node =
    ( Int, Int )
