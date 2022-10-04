module IR.Expr.Fixtures exposing (..)

import IR.Expr exposing (Expr(..))


two : Expr
two =
    -- \s z -> s (s z)
    Lambda "s" <|
        Lambda "z" <|
            Apply (Variable "s") (Apply (Variable "s") (Variable "z"))


compose : Expr
compose =
    -- \f g a -> g (f a)
    Lambda "f" <|
        Lambda "g" <|
            Lambda "a" <|
                Apply (Variable "g") (Apply (Variable "f") (Variable "a"))


apply2 : Expr
apply2 =
    -- \f a b -> f a b
    Lambda "f" <|
        Lambda "a" <|
            Lambda "b" <|
                Apply (Apply (Variable "f") (Variable "a")) (Variable "b")


discard : Expr
discard =
    Lambda "a" <|
        Closure "b" <|
            Variable "a"


always : Expr
always =
    Lambda "a" <|
        Lambda "b" <|
            Variable "a"


always2 : Expr
always2 =
    Closure "a" <|
        Lambda "b" <|
            Variable "b"
