module Lang.Infer.Error exposing (..)

{-|

@docs Error

-}

import Lang.Infer.Error.Internal as Internal


type alias Error =
    Internal.Error


-- TODO: toString