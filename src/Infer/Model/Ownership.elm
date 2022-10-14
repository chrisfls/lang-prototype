module Infer.Model.Ownership exposing (Ownership, empty, hasAny, insert, isAvailable, remove, use)

import Set exposing (Set)



-- TODO: start using IntDict


type alias Ownership =
    { linear : Set String
    , unused : Set String
    }


empty : Ownership
empty =
    { linear = Set.empty
    , unused = Set.empty
    }


insert : String -> Ownership -> Ownership
insert name { linear, unused } =
    { linear = Set.insert name linear
    , unused = Set.insert name unused
    }


use : String -> Ownership -> Ownership
use name { linear, unused } =
    { linear = linear
    , unused = Set.remove name unused
    }


remove : String -> Ownership -> Ownership
remove name { linear, unused } =
    { linear = Set.remove name linear
    , unused = Set.remove name unused
    }


hasAny : Ownership -> Bool
hasAny { linear } =
    not (Set.isEmpty linear)


isAvailable : String -> Ownership -> Bool
isAvailable name scope =
    if Set.member name scope.linear then
        Set.member name scope.unused

    else
        True
