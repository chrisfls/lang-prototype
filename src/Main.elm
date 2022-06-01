module Main exposing (..)

import Browser
import Html exposing (text)
import Bidir2 as Bidir


main =
    let
        ssz =
            Bidir.lam "s" (\s -> Bidir.lam "z" (\z -> Bidir.app s (Bidir.app s z)))

        _ =
            Bidir.check ssz Bidir.empty
                |> Result.map (\(a,_) -> Bidir.toStringT a)
                |> Debug.log "LANG2"

                
        -- compose =
        --     Bidir.lam "f" (\f -> Bidir.lam "g" (\g -> Bidir.lam "a" (\a -> Bidir.app g (Bidir.app f a))))
        -- _ =
        --     Bidir.check compose Bidir.empty
        --         |> Result.map (\(a,_) -> Bidir.toStringT a)
        --         |> Debug.log "LANG2"
    in
    Browser.sandbox { init = (), update = (always (always ())), view = always (text "") }
