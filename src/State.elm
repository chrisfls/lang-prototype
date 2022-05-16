module State exposing (..)

import Basics.Extra exposing (uncurry)


type alias State value state =
    state -> ( value, state )


empty : value -> State value state
empty =
    Tuple.pair


map : (a -> value) -> State a state -> State value state
map f g s =
    Tuple.mapFirst f (g s)


andThen : (a -> State value state) -> State a state -> State value state
andThen f g s =
    uncurry f (g s)


andMap : State a state -> State (a -> value) state -> State value state
andMap f g s0 =
    let
        ( a, s1 ) =
            f s0

        ( b, s2 ) =
            g s1
    in
    ( b a, s2 )

map2 : (a -> b -> value) -> (State a state) -> (State b state) -> State value state
map2 f a b =
    andMap b (map f a)


run : state -> State value state -> ( value, state )
run =
    (|>)

unwrap : state -> State value state -> value
unwrap s f =
    Tuple.first (f s)

sequence : List (State value state) -> State (List value) state
sequence =
    List.foldr (map2 (::)) (empty [])

