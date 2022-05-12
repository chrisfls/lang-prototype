module Main exposing (..)

import Browser
import Dict
import Html exposing (text)
import Infer
import Infer.Expression as Expr
import Infer.Monad as Monad


main =
    let
        _ =
            Expr.Name "z"
                |> Expr.Call (Expr.Name "s")
                |> Expr.Call (Expr.Name "s")
                |> Expr.Lambda "z"
                |> Expr.Lambda "s"
                |> Infer.typeOf Dict.empty
                |> Monad.finalValue 0
                |> Debug.log "Infer"
    in
    Browser.sandbox { init = (), update = update, view = view }


update _ _ =
    ()


view _ =
    text ""
