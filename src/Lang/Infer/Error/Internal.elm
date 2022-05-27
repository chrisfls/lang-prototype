module Lang.Infer.Error.Internal exposing (..)

-- TODO: review

import Lang.Canonical.Name exposing (Name)
import Lang.Canonical.Type.Internal exposing (Type)


type Error
    = NotFound Name
    | Recursion Int Type
    | Mismatch Type Type



-- TODO: stringify
