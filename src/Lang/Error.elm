module Lang.Error exposing (..)

{-|

@docs Error

-}

import Lang.Error.Internal as Internal


type alias Error =
    Internal.Error


-- TODO: toString