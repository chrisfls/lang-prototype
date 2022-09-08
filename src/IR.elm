module IR exposing
    ( IR(..)
    , Defs(..)
    , Path(..)
    , Privacy(..)
    )

{-|

@docs IR
@docs Defs
@docs Path
@docs Privacy

-}

import IR.Expr exposing (Expr, ExprName)
import IR.ModuleName exposing (ModuleName)
import IR.Spec exposing (Spec, SpecName)


type IR
    = Deps ModuleName Path IR
    | Defs Defs



-- TODO: verify if forall should be added
--       to DefSpec or to Spec itself
-- TODO: DefMacro


type Defs
    = DefSpec Privacy SpecName Spec Defs
    | DefExpr Privacy ExprName Expr Defs
    | DefModule


type Path
    = Local String
    | Http String
    | Git


type Privacy
    = Public
    | Private
