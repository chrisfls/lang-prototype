module State exposing (..)


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
    let
        ( a, b ) =
            g s
    in
    f a b


andMap : State a state -> State (a -> value) state -> State value state
andMap f g s0 =
    let
        ( a, s1 ) =
            f s0

        ( b, s2 ) =
            g s1
    in
    ( b a, s2 )


map2 : (a -> b -> value) -> State a state -> State b state -> State value state
map2 f a b =
    andMap b (map f a)


map3 : (a -> b -> c -> value) -> State a state -> State b state -> State c state -> State value state
map3 f a b c =
    andMap c (map2 f a b)


run : state -> State value state -> ( value, state )
run =
    (|>)


unwrap : state -> State value state -> value
unwrap s f =
    Tuple.first (f s)


sequence : List (State value state) -> State (List value) state
sequence =
    List.foldr (map2 (::)) (empty [])
