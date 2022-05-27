module Main exposing (..)

import Browser
import Html exposing (text)
import Lang.Canonical.Name as Name
import Lang.Canonical.Expr as Expr
import Lang.Infer as Infer
import Lang.Infer.Env as TypeEnv


main =
    let
        expr =
            Expr.Lam (Name.fromString "s") (\s -> Expr.Lam (Name.fromString "z") (\z -> Expr.App s (Expr.App s z)))

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
