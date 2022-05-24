module StateResult exposing
    ( StateResult
    , andMap
    , andThen
    , empty
    , error
    , fromState
    , map
    , map2
    , map3
    , run
    , sequence
    , unwrap
    )

import Basics.Extra exposing (flip)
import State exposing (State)


type alias StateResult error value state =
    State (Result error value) state


empty : value -> StateResult error value state
empty =
    Ok >> State.empty


fromState : State value state -> StateResult error value state
fromState f s =
    let
        ( a, s_ ) =
            f s
    in
    ( Ok a, s_ )

error : error -> StateResult error value state
error =
    Err >> State.empty


map : (a -> value) -> StateResult error a state -> StateResult error value state
map =
    Result.map >> State.map


andThen : (a -> StateResult error value state) -> StateResult error a state -> StateResult error value state
andThen =
    andThenHelp >> State.andThen


andThenHelp : (a -> StateResult error value state) -> Result error a -> StateResult error value state
andThenHelp f r =
    case r of
        Ok a ->
            f a

        Err e ->
            State.empty (Err e)


andMap : StateResult error a state -> StateResult error (a -> value) state -> StateResult error value state
andMap =
    flip map >> andThen


map2 : (a -> b -> value) -> StateResult error a state -> StateResult error b state -> StateResult error value state
map2 f a b =
    andMap b (map f a)


map3 : (a -> b -> c -> value) -> StateResult error a state -> StateResult error b state -> StateResult error c state -> StateResult error value state
map3 f a b c =
    andMap c (map2 f a b)


run : state -> StateResult error value state -> ( Result error value, state )
run =
    State.run


unwrap : state -> StateResult error value state -> Result error value
unwrap =
    State.unwrap


sequence : List (StateResult error value state) -> StateResult error (List value) state
sequence =
    List.foldr (map2 (::)) (empty [])
