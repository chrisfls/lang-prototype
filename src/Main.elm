module Main exposing (..)

import Browser
import Dict
import Html exposing (text)
import Lang
import Lang.Expression as Expr
import Lang.Monad as Monad


main =
    let
        _ =
            Expr.Lambda "s" (\s -> Expr.Lambda "z" (\z -> (Expr.Call s (Expr.Call s z))))
                |> Lang.typeOf Dict.empty
                |> Monad.finalValue 0
                |> Debug.log "Lang"
    in
    Browser.sandbox { init = (), update = update, view = view }


update _ _ =
    ()


view _ =
    text ""
