module IR.Expr exposing
    ( Expr(..)
    , ExprName(..), ExprNameString
    )

{-|

@docs Expr
@docs ExprName, ExprNameString

-}

import IR.ModuleName exposing (ModuleName)
import IR.Spec exposing (Spec)


{-| Expressions.

Core lambda calculus:

  - Variable
  - Lambda
  - Apply

Type checking extension:

  - Specify

Module system extension:

  - ModuleExprAt

-}
type Expr
    = Variable ExprName
    | Lambda ExprName Expr
    | Apply Expr Expr
    | Specify Spec Expr
    | ModuleExprAt ModuleName ExprName


type ExprName
    = ExprName ExprNameString


type alias ExprNameString =
    String
