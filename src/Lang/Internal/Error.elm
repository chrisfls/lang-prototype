module Lang.Internal.Error exposing (..)

import Lang.Internal.Canonical.Type exposing (Type)


type Error
    = NotFound String
    | Recursion Int Type
    | Mismatch Type Type
