module Main exposing (..)

import Browser
import Dict
import Html exposing (text)
import Lang2
import Lang2.Monad as Monad
import Lang2.Syntax.Expr as Expr
import State

type alias Person = { a: Bool, b: Int, c: String }

main =
    let
        _ =
            Expr.Lam "s" (\s -> Expr.Lam "z" (\z -> Expr.App s (Expr.App s z)))
                |> Lang2.typeOf Dict.empty
                |> Monad.finalValue 0
                |> Debug.log "Lang"
    in
    Browser.sandbox { init = (), update = update, view = view }


update _ _ =
    ()


view _ =
    text ""
