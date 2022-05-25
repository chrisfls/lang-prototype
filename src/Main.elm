module Main exposing (..)

import Browser
import Html exposing (text)
import Lang.Canonical.Expr as Expr exposing (Name)
import Lang.Infer as Infer
import Lang.Canonical.Type.Env as TypeEnv


main =
    let
        expr =
            Expr.Lam "s" (\s -> Expr.Lam "z" (\z -> Expr.App s (Expr.App s z)))

        _ =
            Infer.typeOf expr TypeEnv.empty 0
                |> Tuple.first
                |> Debug.log "Lang"
    in
    Browser.sandbox { init = (), update = update, view = view }


update _ _ =
    ()


view _ =
    text ""
