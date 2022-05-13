module Main exposing (..)

import Browser
import Dict
import Html exposing (text)
import Lang
import Lang.Syntax.Expr as Expr
import Lang.Monad as Monad


main =
    let
        _ =
            Expr.Lam "s" (\s -> Expr.Lam "z" (\z -> (Expr.App s (Expr.App s z))))
                |> Lang.typeOf Dict.empty
                |> Monad.finalValue 0
                |> Debug.log "Lang"
    in
    Browser.sandbox { init = (), update = update, view = view }


update _ _ =
    ()


view _ =
    text ""
