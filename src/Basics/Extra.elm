module Basics.Extra exposing (..)

chain : a -> (a -> b) -> b
chain =
    (|>)

flip : (a -> b -> c) -> b -> a -> c
flip f a b =
    f b a

uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f (a, b) =
    f a b