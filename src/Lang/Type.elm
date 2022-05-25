module Lang.Type exposing (Type)

{-|

@docs Type

-}

import Lang.Internal.Canonical.Type as Internal


type alias Type =
    Internal.Type
