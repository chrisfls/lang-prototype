module Infer.Monad exposing (..)

{-|


# Construction

@docs pure, err, Monad, fromResult


# Mapping

@docs andThen, map, map2, map3, map4, andMap


# Combining

@docs combine, finalValue

-}


{-| Represents a stateful computation that can fail.
-}
type alias Monad a =
    Int -> ( Result String a, Int )


{-| Put a value into the monad. Will not advance the fresh name supply nor cause an error.
-}
pure : a -> (Int -> ( Result String a, Int ))
pure x =
    (\value s -> ( value, s )) (Ok x)


{-| Represents a failed computation.
-}
err : String -> (Int -> ( Result String a, Int ))
err e =
    (\value s -> ( value, s )) (Err e)


{-| Un-specialize a Result.
-}
fromResult : Result String a -> (Int -> ( Result String a, Int ))
fromResult res =
    (\value s -> ( value, s )) res


{-| `map` for this particular monad.
-}
map : (a -> value) -> (Int -> ( Result String a, Int )) -> Monad value
map f =
    (\f_ step currentState ->
        let
            ( value, newState ) =
                step currentState
        in
        ( f_ value, newState )
    )
        (Result.map f)


{-| `andThen` for this particular monad.
-}
andThen : (a -> Monad b) -> (Int -> ( Result String a, Int )) -> Monad b
andThen f =
    (\f_ h s ->
        let
            ( a, newState ) =
                h s
        in
        f_ a newState
    )
        (\r ->
            case r of
                Ok v ->
                    f v

                Err e ->
                    (\value s -> ( value, s )) <| Err e
        )


{-| map over one more value. Arguments are reversed in order to work
well with `<|`.
-}
andMap : Monad y -> Monad (y -> z) -> Monad z
andMap y =
    andThen (\g -> map g y)


{-| `map2` for this particular monad.
-}
map2 : (a -> b -> c) -> (Int -> ( Result String a, Int )) -> Monad b -> Monad c
map2 f x y =
    map f x
        |> andMap y


{-| `map3` for this particular monad.
-}
map3 : (a -> b -> c -> d) -> (Int -> ( Result String a, Int )) -> Monad b -> Monad c -> Monad d
map3 f a b c =
    map2 f a b
        |> andMap c


{-| `map4` for this particular monad.
-}
map4 : (a -> b -> c -> d -> e) -> (Int -> ( Result String a, Int )) -> Monad b -> Monad c -> Monad d -> Monad e
map4 f a b c d =
    map3 f a b c
        |> andMap d


{-| Lifts the monads out of a list.
-}
combine : List (Int -> ( Result String a, Int )) -> Int -> ( Result String (List a), Int )
combine =
    List.foldr (map2 (::)) (pure [])


{-| Computes the value of a computation.
-}
finalValue : Int -> (Int -> ( Result String a, Int )) -> Result String a
finalValue initialState =
    Tuple.first << run initialState


run : Int -> (Int -> ( Result String a, Int )) -> ( Result String a, Int )
run initialState s =
    s initialState


advance : (Int -> ( Result String a, Int )) -> (Int -> ( Result String a, Int ))
advance f =
    f
