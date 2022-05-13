module Lang.Expr exposing (..)

import Lang.Type as Type exposing (Type(..))

type Term
    = Var String
    | LamAff String (Term -> Term)
    | LamBox String (Term -> Term)
    | Apply Term Term
    | Box Term
    | Ann Term Type
