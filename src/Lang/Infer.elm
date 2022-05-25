module Lang.Infer exposing (..)

-- TODO: review

import Lang.Canonical.Expr exposing (Expr)
import Lang.Canonical.Type exposing (Type)
import Lang.Infer.Constraint as Constraint exposing (Constraint)
import Lang.Infer.Env exposing (Env)
import Lang.Infer.Error exposing (Error)
import Lang.Infer.State exposing (State)
import Lang.Infer.Subst as Subst exposing (Subst)


types : Expr -> Int -> Env -> Bool
types exp s env =
    typeOf exp env s
        |> Tuple.first
        |> Result.map (always True)
        |> Result.withDefault False


typeOf : Expr -> Env -> State Error ( Type, Type -> Type ) Int
typeOf exp env =
    Constraint.generate exp env
        |> Lang.Infer.State.andThen
            (\( t, cs ) ->
                solve Subst.empty cs
                    |> Result.map (\s -> ( Subst.substitute s t, Subst.substitute s ))
                    |> Lang.Infer.State.fromResult
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
