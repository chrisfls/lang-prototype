module Lang.Infer.State exposing
    ( State
    , andMap
    , andThen
    , empty
    , error
    , fromResult
    , map
    , map2
    , map3
    , run
    , sequence
    , unwrap
    )


type alias State error value state =
    state -> ( Result error value, state )


empty : value -> State error value state
empty a s =
    ( Ok a, s )


fromResult : Result error value -> State error value state
fromResult =
    Tuple.pair


error : error -> State error value state
error a s =
    ( Err a, s )


map : (a -> value) -> State error a state -> State error value state
map f g s =
    let
        ( r, s_ ) =
            g s
    in
    case r of
        Ok a ->
            ( Ok (f a), s_ )

        Err e ->
            ( Err e, s_ )


andThen : (a -> State error value state) -> State error a state -> State error value state
andThen f g s =
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
andMap f g s0 =
    let
        ( r1, s1 ) =
            f s0
    in
    case r1 of
        Ok a ->
            let
                ( r2, s2 ) =
                    g s1
            in
            case r2 of
                Ok b ->
                    ( Ok (b a), s2 )

                Err e ->
                    ( Err e, s2 )

        Err e ->
            ( Err e, s1 )


map2 : (a -> b -> value) -> State error a state -> State error b state -> State error value state
map2 f a b =
    andMap b (map f a)


map3 : (a -> b -> c -> value) -> State error a state -> State error b state -> State error c state -> State error value state
map3 f a b c =
    andMap c (map2 f a b)


run : state -> State error value state -> ( Result error value, state )
run =
    (|>)


unwrap : state -> State error value state -> Result error value
unwrap s f =
    Tuple.first (f s)


sequence : List (State error value state) -> State error (List value) state
sequence =
    List.foldr (map2 (::)) (empty [])
