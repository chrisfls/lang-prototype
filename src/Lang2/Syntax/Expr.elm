module Lang2.Syntax.Expr exposing (Expr(..))

import Lang2.Syntax.Type exposing (Type)


type alias Name =
    String


type Expr
    = Var Name
    | Lit Type
    | Lam Name (Expr -> Expr)
    | App Expr Expr
    | Let Name Expr Expr
    | Spy Expr Int



-- TODO
-- * add Lit
-- * add Fix
-- * add Op
-- * rem Let
