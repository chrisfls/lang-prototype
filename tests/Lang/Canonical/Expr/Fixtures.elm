module Lang.Canonical.Expr.Fixtures exposing (..)

import Lang.Canonical.Expr exposing (Expr(..))
import Lang.Infer.Error exposing (Error(..))
import Dict


two : Expr
two =
    -- \s z -> s (s z)
    Lam "s" <|
        \s ->
            Lam "z" <|
                \z -> App s (App s z)


compose : Expr
compose =
    -- \f g a -> g (f a)
    Lam "f" <|
        \f ->
            Lam "g" <|
                \g ->
                    Lam "a" <|
                        \a -> App g (App f a)


apply2 : Expr
apply2 =
    -- \f a b -> f a b
    Lam "f" <|
        \f ->
            Lam "a" <|
                \a ->
                    Lam "b" <|
                        \b -> App (App f a) b


pair : Expr
pair =
    -- \a b -> (a, b)
    Lam "a" <|
        \a ->
            Lam "b" <|
                \b -> Tup [ a, b ]


record : Expr
record =
    -- \a b ->  { a = a, b = b }
    Lam "a" <|
        \a ->
            Lam "b" <|
                \b ->
                    Dict.fromList
                        [ ( "a", a )
                        , ( "b", b )
                        ]
                        |> Rec
