module Lang.Type exposing (..)

import Set exposing (Set)

-- TODO: opaque and other types


type Type
    = TAny Int
    | TArrow Type Type
    | TBox Type

variables : Type -> Set Int
variables typ =
    case typ of
        TAny idx ->
            Set.singleton idx

        TArrow arg ret ->
            Set.union (variables arg) (variables ret)

        TBox box ->
            variables box