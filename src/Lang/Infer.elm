module Lang.Infer exposing (..)

import Lang.Canonical.Expr exposing (Expr)
import Lang.Canonical.Type exposing (Type)
import Lang.Infer.Error exposing (Error)
import Lang.Infer.Constraint as Constraint exposing (Constraint)
import Lang.Infer.Subst as Subst exposing (Subst)
import Lang.Infer.Env exposing (TypeEnv)
import Lang.Infer.StateResult exposing (StateResult)


types : Expr -> Int -> TypeEnv -> Bool
types exp s env =
    typeOf exp env s
        |> Tuple.first
        |> Result.map (always True)
        |> Result.withDefault False


typeOf : Expr -> TypeEnv -> StateResult Error ( Type, Type -> Type ) Int
typeOf exp env =
    Constraint.generate exp env
        |> Lang.Infer.StateResult.andThen
            (\( t, cs ) ->
                solve Subst.empty cs
                    |> Result.map (\s -> ( Subst.substitute s t, Subst.substitute s ))
                    |> Lang.Infer.StateResult.fromResult
            )


solve : Subst -> List Constraint -> Result Error Subst
solve substitution constraints =
    case constraints of
        [] ->
            Ok substitution

        ( t1, t2 ) :: tail ->
            Subst.unify t1 t2
                |> Result.andThen
                    (\new ->
                        solve
                            (Subst.app new substitution)
                            (List.map (substituteConstraint new) tail)
                    )


substituteConstraint : Subst -> Constraint -> Constraint
substituteConstraint substitution ( l, r ) =
    let
        f =
            Subst.substitute substitution
    in
    ( f l, f r )
