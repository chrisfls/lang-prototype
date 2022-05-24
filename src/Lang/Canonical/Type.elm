module Lang.Canonical.Type exposing (Type(..), freshTVar, variables)

import Set exposing (Set)
import State exposing (State)


type Type
    = TArr Type Type
    | TCon String (List Type)
    | TVar Int
    | TTuple (List Type)


freshTVar : State Type Int
freshTVar state =
    ( TVar state, state + 1 )


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


variablesFromList : List Type -> Set Int
variablesFromList =
    List.map variables
        >> List.foldl Set.union Set.empty
