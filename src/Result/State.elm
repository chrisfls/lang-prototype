module Result.State exposing (..)

import Basics.Extra exposing (flip)
import State


type alias State error value state =
    State.State (Result error value) state


empty : value -> State error value state
empty =
    Ok >> State.empty


map : (a -> value) -> State error a state -> State error value state
map =
    Result.map >> State.map


andThen : (a -> State error value state) -> State error a state -> State error value state
andThen f g s =
    -- andThen f =
    --     State.andThen
    --         (\r ->
    --             case r of
    --                 Ok v ->
    --                     f v
    --                 Err e ->
    --                     State.empty <| Err e
    --         )
    let
        ( r, s_ ) =
            g s
    in
    case r of
        Ok a ->
            f a s_

        Err e ->
            ( Err e, s_ )


andMap : State error a state -> State error (a -> value) state -> State error value state
andMap =
    flip map >> andThen


map2 : (a -> b -> value) -> State error a state -> State error b state -> State error value state
map2 f a b =
    andMap b (map f a)


run : state -> State error value state -> ( Result error value, state )
run =
    State.run


unwrap : state -> State error value state -> Result error value
unwrap =
    State.unwrap


sequence : List (State error value state) -> State error (List value) state
sequence =
    List.foldr (map2 (::)) (empty [])
