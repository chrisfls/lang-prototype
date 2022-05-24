module Lang.Infer exposing (..)

import Lang.Canonical.Expr exposing (Expr)
import Lang.Canonical.Type exposing (Type)
import Lang.Constraint as Constraint exposing (Constraint)
import Lang.Inference.Error exposing (Error)
import Lang.Substitution as Substitution exposing (Substitution)
import Lang.TypeEnv exposing (TypeEnv)
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


solve : Substitution -> List Constraint -> Result Error Substitution
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


substituteConstraint : Substitution -> Constraint -> Constraint
substituteConstraint substitution ( l, r ) =
    let
        f =
            Substitution.substitute substitution
    in
    ( f l, f r )
