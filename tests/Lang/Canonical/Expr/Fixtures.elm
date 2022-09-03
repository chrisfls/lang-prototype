module Lang.Canonical.Expr.Fixtures exposing (..)

import Dict
import Lang.Canonical.Expr exposing (BuiltinValue(..), Expr(..))
import Lang.Infer.Error exposing (Error(..))


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


recordUpdate : Expr
recordUpdate =
    -- \a b c -> { c | a = a, b = b }
    Lam "a" <|
        \a ->
            Lam "b" <|
                \b ->
                    Lam "c" <|
                        \c ->
                            Dict.fromList
                                [ ( "a", a )
                                , ( "b", b )
                                ]
                                |> Upd c


alwaysUnit : Expr
alwaysUnit =
    -- \a -> ()
    Lam "a" <|
        \_ -> Bul UnitVal


alwaysTrue : Expr
alwaysTrue =
    -- \a -> True
    Lam "a" <|
        \_ -> Bul TrueVal


alwaysFalse : Expr
alwaysFalse =
    -- \a -> False
    Lam "a" <|
        \_ -> Bul FalseVal


alwaysInt : Expr
alwaysInt =
    -- \a -> 0
    Lam "a" <|
        \_ -> Bul <| IntVal 0


alwaysFloat : Expr
alwaysFloat =
    -- \a -> 0.5
    Lam "a" <|
        \_ -> Bul <| FloatVal 0.5


alwaysString : Expr
alwaysString =
    -- \a -> "example"
    Lam "a" <|
        \_ -> Bul <| StringVal "example"

alwaysSome : Expr
alwaysSome =
    Lam "a" <|
        \_ ->
            App (Var "Some") (Bul UnitVal)


alwaysNone : Expr
alwaysNone =
    Lam "a" <|
        \_ ->
            (Var "None")
