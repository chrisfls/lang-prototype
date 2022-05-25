module Lang.Infer exposing (..)

import Lang.Canonical.Expr exposing (Expr)
import Lang.Canonical.Type.Internal exposing (Type)
import Lang.Infer.Constraint as Constraint exposing (Constraint)
import Lang.Infer.Error.Internal exposing (Error)
import Lang.Canonical.Type.Subst as Substitution exposing (Subst)
import Lang.Canonical.Type.Env exposing (TypeEnv)
import StateResult exposing (StateResult)


types : Expr -> Int -> TypeEnv -> Bool
types exp s env =
    typeOf exp env s
        |> Tuple.first
        |> Result.map (always True)
        |> Result.withDefault False


typeOf : Expr -> TypeEnv -> StateResult Error ( Type, Type -> Type ) Int
typeOf exp env =
    Constraint.generate exp env
        |> StateResult.andThen
            (\( t, cs ) ->
                solve Substitution.empty cs
                    |> Result.map (\s -> ( Substitution.substitute s t, Substitution.substitute s ))
                    |> StateResult.fromResult
            )


solve : Subst -> List Constraint -> Result Error Subst
solve substitution constraints =
    case constraints of
        [] ->
            Ok substitution

        ( t1, t2 ) :: tail ->
            Substitution.unify t1 t2
                |> Result.andThen
                    (\new ->
                        solve
                            (Substitution.app new substitution)
                            (List.map (substituteConstraint new) tail)
                    )


substituteConstraint : Subst -> Constraint -> Constraint
substituteConstraint substitution ( l, r ) =
    let
        f =
            Substitution.substitute substitution
    in
    ( f l, f r )
