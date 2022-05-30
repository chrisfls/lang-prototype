module Lang.Infer.Error exposing (..)

import Lang.Canonical.Name exposing (Name)
import Lang.Canonical.Type exposing (Type)


type Error
    = NotFound Name
    | Recursion Int Type
    | Mismatch Type Type



-- TODO: stringify
