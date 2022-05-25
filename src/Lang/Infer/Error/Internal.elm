module Lang.Infer.Error.Internal exposing (..)

-- TODO: review

import Lang.Canonical.Type.Internal exposing (Type)


type Error
    = NotFound String
    | Recursion Int Type
    | Mismatch Type Type



-- TODO: stringify
