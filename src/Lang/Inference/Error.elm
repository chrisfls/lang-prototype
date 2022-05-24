module Lang.Inference.Error exposing (..)

import Lang.Canonical.Type exposing (Type)


type Error
    = NotFound String
    | Recursion Int Type
    | Mismatch Type Type
