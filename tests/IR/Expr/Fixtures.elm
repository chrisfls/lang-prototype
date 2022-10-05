module IR.Expr.Fixtures exposing (..)

import IR.Expr exposing (Expr(..))


two : Expr
two =
    -- \s z -> s (s z)
    Lambda Nothing "s" <|
        Lambda Nothing "z" <|
            Apply (Variable "s") (Apply (Variable "s") (Variable "z"))


compose : Expr
compose =
    -- \f g a -> g (f a)
    Lambda Nothing "f" <|
        Lambda Nothing "g" <|
            Lambda Nothing "a" <|
                Apply (Variable "g") (Apply (Variable "f") (Variable "a"))


apply2 : Expr
apply2 =
    -- \f a b -> f a b
    Lambda Nothing "f" <|
        Lambda Nothing "a" <|
            Lambda Nothing "b" <|
                Apply (Apply (Variable "f") (Variable "a")) (Variable "b")


discard : Expr
discard =
    Lambda Nothing "a" <|
        Lambda (Just True) "b" <|
            Variable "a"


always : Expr
always =
    Lambda Nothing "a" <|
        Lambda Nothing "b" <|
            Variable "a"


always2 : Expr
always2 =
    Lambda (Just True) "a" <|
        Lambda Nothing "b" <|
            Variable "b"
