module Lang.Canonical.Type.Internal exposing (..)

import Dict exposing (Dict)
import Lang.Canonical.Name exposing (Name)
import Set exposing (Set)


type Type
    = TArr Type Type
    | TCon Name (List Type)
    | TVar Int
    | TTuple (List Type)
    | TRecord (Dict Name Type)


variables : Type -> Set Int
variables t =
    case t of
        TArr l r ->
            Set.union (variables l) (variables r)

        TCon _ args ->
            variablesFromList args

        TVar x ->
            Set.singleton x

        TTuple types ->
            variablesFromList types

        TRecord d ->
            variablesFromList (Dict.values d)


variablesFromList : List Type -> Set Int
variablesFromList =
    List.map variables
        >> List.foldl Set.union Set.empty
