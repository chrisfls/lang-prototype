module Lang.Monad exposing (..)

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
pure : a -> Int -> ( Result String a, Int )
pure x s =
    ( Ok x, s )


{-| Represents a failed computation.
-}
err : String -> Int -> ( Result String a, Int )
err e s =
    ( Err e, s )


{-| Un-specialize a Result.
-}
fromResult : Result String a -> Int -> ( Result String a, Int )
fromResult res s =
    ( res, s )


{-| `map` for this particular monad.
-}
map : (a -> value) -> (Int -> ( Result String a, Int )) -> Int -> ( Result String value, Int )
map f step s =
    let
        ( value, s_ ) =
            step s
    in
    ( Result.map f value, s_ )


{-| `andThen` for this particular monad.
-}
andThen : (a -> Monad b) -> (Int -> ( Result String a, Int )) -> Monad b
andThen f h s =
    let
        ( a, s_ ) =
            h s
    in
    case a of
        Ok v ->
            f v s_

        Err e ->
            ( Err e, s_ )


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
    andMap y (map f x)


{-| `map3` for this particular monad.
-}
map3 : (a -> b -> c -> d) -> (Int -> ( Result String a, Int )) -> Monad b -> Monad c -> Monad d
map3 f a b c =
    andMap c (map2 f a b)


{-| `map4` for this particular monad.
-}
map4 : (a -> b -> c -> d -> e) -> (Int -> ( Result String a, Int )) -> Monad b -> Monad c -> Monad d -> Monad e
map4 f a b c d =
    andMap d (map3 f a b c)


{-| Lifts the monads out of a list.
-}
combine : List (Int -> ( Result String a, Int )) -> Int -> ( Result String (List a), Int )
combine =
    List.foldr (map2 (::)) (pure [])


{-| Computes the value of a computation.
-}
finalValue : Int -> (Int -> ( Result String a, Int )) -> Result String a
finalValue s x =
    Tuple.first (run s x)


run : Int -> (Int -> ( Result String a, Int )) -> ( Result String a, Int )
run s step =
    step s


advance : (Int -> ( Result String a, Int )) -> (Int -> ( Result String a, Int ))
advance f =
    f
