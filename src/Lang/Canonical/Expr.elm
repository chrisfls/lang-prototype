module Lang.Canonical.Expr exposing (..)

import Lang.Canonical.Name exposing (Name)
import Lang.Canonical.Type exposing (Type)


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
