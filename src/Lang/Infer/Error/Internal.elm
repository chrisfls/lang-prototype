module Lang.Infer.Error.Internal exposing (..)

import Lang.Canonical.Type.Internal exposing (Type)


type Error
    = NotFound String
    | Recursion Int Type
    | Mismatch Type Type


-- TODO: stringify