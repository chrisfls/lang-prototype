module Main exposing (..)

import Bidir2 as Bidir
import Browser
import Html exposing (text)


main =
    let
        -- expr =
        --     Bidir.lam "s" (\s -> Bidir.lam "z" (\z -> Bidir.app s (Bidir.app s z)))
        -- expr =
        --     Bidir.lam "f" (\f -> Bidir.lam "g" (\g -> Bidir.lam "a" (\a -> Bidir.app g (Bidir.app f a))))

        expr =
            Bidir.lam "f" (\f -> Bidir.lam "a" (\a -> Bidir.lam "b" (\b -> Bidir.app (Bidir.app f a) b)))
        _ =
            Bidir.check expr Bidir.empty
                |> Result.map (\( a, _ ) -> Bidir.toStringT a)
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
