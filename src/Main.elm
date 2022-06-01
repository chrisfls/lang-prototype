module Main exposing (..)

import Browser
import Html exposing (text)
import Lang.Canonical.Expr as Expr
import Lang.Canonical.Name as Name
import Lang.Infer as Infer
import Lang.Infer.Env as TypeEnv
import Bidir4 as Bidir


main =
    let
        -- expr =
        --     Expr.Lam (Name.fromString "s") (\s -> Expr.Lam (Name.fromString "z") (\z -> Expr.App s (Expr.App s z)))

        -- _ =
        --     Infer.typeOf expr TypeEnv.empty 0
        --         |> Tuple.first
        --         |> Debug.log "Lang"

        -- expr_ =
        --     Bidir.lam "s" (\s -> Bidir.lam "z" (\z -> Bidir.app s (Bidir.app s z)))
        
        ssz =
            Bidir.lam "s" (\s -> Bidir.lam "z" (\z -> Bidir.app s (Bidir.app s z)))

        compose =
            Bidir.lam "f" (\f -> Bidir.lam "g" (\g -> Bidir.lam "a" (\a -> Bidir.app g (Bidir.app f a))))

        _ =
            Bidir.check ssz Bidir.empty
                |> Result.map (\(a,_) -> Bidir.toStringT a)
                |> Debug.log "LANG2"
        _ =
            Bidir.check compose Bidir.empty
                |> Result.map (\(a,_) -> Bidir.toStringT a)
                |> Debug.log "LANG2"
    in
    Browser.sandbox { init = (), update = update, view = view }


update _ _ =
    ()


view _ =
    text ""



comp : (a -> b) -> (b -> c) -> a -> c
comp f g a =
    g (f a)


ssz_ : (b -> b) -> b -> b
ssz_ s z = 
    s (s z)


-- two s z = s (s z)
-- passando por (\s z -> s (s z))
-- S vai ter type ref var0 (desconhecida)

-- passando por (\z -> s (s z))
-- Z vai ter type ref var1 (desconhecida)

-- passando por s (s z) checa o tipo de (s z)

-- passando por (s z)
-- var0 deve aceitar var1 como param, retorna var2 (desconhecido)

-- voltando para s (s z), agora com var2 que foi o tipo retornado

-- var0 deve aceitar var2 como param, como var0 já deve aceitar var1, var2 deve ser igual var1

-- no final vc termina com:

-- two : (var1 -> var1) -> var1 -> var1 
-- pq var0 vai ser (var1 -> var1)
