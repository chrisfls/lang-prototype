module IR.SpecExpr exposing (..)

import IR.Expr as Expr exposing (Expr)
import IR.Spec exposing (Spec)


type SpecExpr
    = Variable Spec String
    | Lambda Spec Bool String SpecExpr
    | Apply Spec SpecExpr SpecExpr
    | Unborrow Spec String SpecExpr


type alias Linear =
    Maybe Bool


toString : SpecExpr -> String
toString =
    toExpr >> Expr.toString


toSpec : SpecExpr -> Spec
toSpec specExpr =
    case specExpr of
        Variable spec _ ->
            spec

        Lambda spec _ _ _ ->
            spec

        Apply spec _ _ ->
            spec

        Unborrow spec _ _ ->
            spec


toExpr : SpecExpr -> Expr
toExpr specExpr =
    case specExpr of
        Variable _ name ->
            Expr.Variable name

        Lambda _ linear name body ->
            Expr.Lambda (Just linear) name (toExpr body)

        Apply _ function argument ->
            Expr.Apply (toExpr function) (toExpr argument)

        Unborrow _ name body ->
            Expr.Unborrow name (toExpr body)
