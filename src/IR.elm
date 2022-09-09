module IR exposing
    ( IR(..)
    , ApplyArgs
    , Defs(..)
    , Path(..)
    , Privacy(..)
    )

{-|

@docs IR
@docs ApplyArgs
@docs Defs
@docs Path
@docs Privacy

-}

import IR.Expr exposing (Expr, ExprName)
import IR.ModuleName exposing (ModuleName)
import IR.Spec exposing (Spec, SpecName)


type IR
    = Argument ModuleName (Maybe Spec) IR
    | Import ModuleName Path ApplyArgs IR
    | Forward Expr Path ApplyArgs IR
    | Body Defs


type alias ApplyArgs =
    List ModuleName



-- TODO: verify if forall should be added
--       to DefSpec or to Spec itself


type Defs
    = DefSpec Privacy SpecName Spec Defs
    | DefMacro Privacy ExprName Expr Defs
    | DefExpr Privacy ExprName Expr Defs
    | ApplyMacro Expr Defs
    | DefModule


type Path
    = Local String
    | Http String
    | Git


type Privacy
    = Public
    | Private
