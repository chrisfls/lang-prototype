module IR exposing
    ( IR(..)
    , Defs
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



-- TODO: verify if forall should be added
--       to DefSpec or to Spec itself


type IR
    = Import ModuleName Path IR
    | Variable ModuleName
    | Lambda ModuleName (Maybe Spec) IR
    | Apply IR IR
    | Module Defs


type Defs
    = Macro IR IR
    | DefSpec Privacy SpecName Spec Defs
    | DefMacro Privacy ExprName Expr Defs
    | DefExpr Privacy ExprName Expr Defs
    | DefModule


type Path
    = Local String
    | Http String
    | Git


type Privacy
    = Public
    | Private
