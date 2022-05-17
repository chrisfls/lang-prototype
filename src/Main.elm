module Main exposing (..)

import Browser
import Dict
import Html exposing (text)
import Lang
import Lang.Monad as Monad
import Lang.Syntax.Expr as Expr
import State

type alias Person = { a: Bool, b: Int, c: String }

a : number -> Person
a s =
    always (Person, 1)
        |> State.andMap (always (False, 2))
        |> State.andMap (always (12, 2))
        |> State.andMap (always ("12", 2))
        |> State.unwrap s


main =
    let
        _ =
            Expr.Lam "s" (\s -> Expr.Lam "z" (\z -> Expr.App s (Expr.App s z)))
                |> Lang.typeOf Dict.empty
                |> Monad.finalValue 0
                |> Debug.log "Lang"
    in
    Browser.sandbox { init = (), update = update, view = view }


update _ _ =
    ()


view _ =
    text ""
