module Lang.Constraint exposing (..)

import Lang.Expr exposing (Type(..))


type Constraint
    = Constraint Type Type
