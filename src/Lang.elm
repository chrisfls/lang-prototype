module Lang exposing (typeOf)

{-| This is the module implementing type inference. You'll also need at least `Lang.Expression`.

@docs typeOf

-}

import Dict
import Lang.ConstraintGen exposing (Constraint, generateConstraints)
import Lang.Monad as Monad
import Lang.Scheme exposing (Environment)
import Lang.Syntax.Expr exposing (Expr)
import Lang.Syntax.Type as Type exposing (Substitution, Type, app)


types : Environment -> Expr -> Int -> Bool
types env exp s =
    typeOf env exp
        |> Monad.finalValue s
        |> Result.map (always True)
        |> Result.withDefault False


{-| Returns a computation that yields the type of the input expression
with the specified environment.
-}
typeOf : Environment -> Expr -> (Int -> ( Result String ( Type, Type -> Type ), Int ))
typeOf env exp =
    generateConstraints exp env
        |> Monad.andThen
            (\( t, cs ) ->
                solve Dict.empty cs
                    |> Result.map (\s -> ( Type.substitute s t, Type.substitute s ))
                    |> Monad.fromResult
            )


solve : Substitution -> List Constraint -> Result String Substitution
solve substitution constraints =
    case constraints of
        [] ->
            Ok substitution

        ( t1, t2 ) :: tail ->
            Type.unify t1 t2
                |> Result.andThen
                    (\new ->
                        solve
                            (app new substitution)
                            (List.map (substituteConstraint new) tail)
                    )


substituteConstraint : Substitution -> Constraint -> Constraint
substituteConstraint substitution ( l, r ) =
    let
        f =
            Type.substitute substitution
    in
    ( f l, f r )
