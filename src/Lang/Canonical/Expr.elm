module Lang.Canonical.Expr exposing (..)

-- TODO: review

import Lang.Canonical.Type.Internal exposing (Type)


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
