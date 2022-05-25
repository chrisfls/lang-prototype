module Lang.Canonical.Type.Internal exposing (..)

import Set exposing (Set)
import Dict exposing (Dict)
import Lang.Infer.StateResult exposing (StateResult)


type Type
    = TArr Type Type
    | TCon String (List Type)
    | TVar Int
    | TTuple (List Type)
    | TRecord (Dict String Type)


freshTVar : StateResult error Type Int
freshTVar state =
    ( Ok (TVar state), state + 1 )


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
            Dict.values d
                |> variablesFromList

variablesFromList : List Type -> Set Int
variablesFromList =
    List.map variables
        >> List.foldl Set.union Set.empty
