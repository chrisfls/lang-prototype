module Main exposing (..)

import Bidir2 as Bidir exposing (Exp(..), Type(..))
import Browser
import Html exposing (text)


main =
    let
        -- expr =
        --     Lam "s" (\s -> Lam "z" (\z -> App s (App s z)))
        -- expr =
        --     Lam "f" (\f -> Lam "g" (\g -> Lam "a" (\a -> App g (App f a))))
        expr =
            Lam "f" (\f -> Lam "a" (\a -> Lam "b" (\b -> App (App f a) b)))

        -- ann =
        --     TArr (TArr (TVar False 0) (TArr (TVar False 1) (TVar False 2))) (TArr (TVar False 0) (TArr (TVar False 1) (TVar False 2)))

        -- expr_ =
        --     Ann ann expr

        _ =
            Bidir.check expr Bidir.empty
                |> Result.map (\( a, _ ) -> Debug.toString a)
                |> Debug.log "LANG2"
    in
    Browser.sandbox { init = (), update = always (always ()), view = always (text "") }


ssz : (b -> b) -> b -> b
ssz s z =
    -- behaves better
    s (s z)


compose : (a -> b) -> (b -> c) -> a -> c
compose =
    -- behaves the same
    (>>)


apply2 : (b -> c -> a) -> b -> c -> a
apply2 f a b =
    -- wip
    f a b
