module Lang.Infer exposing (..)

import Lang.Canonical.Expr exposing (Expr)
import Lang.Canonical.Type exposing (Type)
import Lang.Infer.Constraint as Constraint exposing (Constraint(..), ConstraintState(..))
import Lang.Infer.Env exposing (Env)
import Lang.Infer.Error exposing (Error)
import Lang.Infer.State as State exposing (State)
import Lang.Infer.Subst as Subst exposing (Subst)


types : Expr -> Int -> Env -> Bool
types exp s env =
    typeOf exp env s
        |> Tuple.first
        |> Result.map (always True)
        |> Result.withDefault False


typeOf : Expr -> Env -> State Error ( Type, Type -> Type ) Int
typeOf exp env =
    State.andThen
        (\(ConstraintState t cs) ->
            solve Subst.empty cs
                |> Result.map (\s -> ( Subst.substitute s t, Subst.substitute s ))
                |> State.fromResult
        )
        (Constraint.generate exp env)


solve : Subst -> List Constraint -> Result Error Subst
solve substitution constraints =
    case constraints of
        [] ->
            Ok substitution

        (Constraint t1 t2) :: tail ->
            Result.andThen
                (\new ->
                    solve
                        (Subst.app new substitution)
                        (List.map (substituteConstraint new) tail)
                )
                (Subst.unify t1 t2)


substituteConstraint : Subst -> Constraint -> Constraint
substituteConstraint substitution (Constraint l r) =
    let
        f =
            Subst.substitute substitution
    in
    Constraint (f l) (f r)
