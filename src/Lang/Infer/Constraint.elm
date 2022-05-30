module Lang.Infer.Constraint exposing (Constraint(..), ConstraintState(..), generate)

import Lang.Canonical.Expr exposing (Expr(..))
import Lang.Canonical.Type as Type exposing (Type)
import Lang.Infer.Env as Env exposing (Env)
import Lang.Infer.Error exposing (Error)
import Lang.Infer.State as State exposing (State)


type Constraint
    = Constraint Type Type


type alias Constraints =
    List Constraint


type ConstraintState
    = ConstraintState Type Constraints


singleton : Type -> ConstraintState
singleton x =
    ConstraintState x []


mapFirst : (Type -> Type) -> ConstraintState -> ConstraintState
mapFirst f (ConstraintState a b) =
    ConstraintState (f a) b


mergeAppConstraints : Type -> ConstraintState -> ConstraintState -> ConstraintState
mergeAppConstraints typ (ConstraintState func funcConstraint) (ConstraintState argm argConstraint) =
    ConstraintState typ
        (funcConstraint ++ argConstraint ++ [ Constraint func (Type.TArr argm typ) ])


generate : Expr -> Env -> State Error ConstraintState Int
generate expr env =
    case expr of
        Var name ->
            State.map singleton (Env.variable name env)

        Lit t ->
            State.empty (singleton t)

        App func argm ->
            State.map3 mergeAppConstraints
                Env.freshTVar
                (generate func env)
                (generate argm env)

        Lam argm body ->
            State.andThen
                (\argmT ->
                    Env.extend argm argmT env
                        |> generate (body (Var argm))
                        |> State.map (mapFirst (\a -> Type.TArr a argmT))
                )
                Env.freshTVar

        Let name value body ->
            State.andThen
                (\(ConstraintState valueT valueC) ->
                    State.map2
                        (\this (ConstraintState bodyT bodyC) ->
                            ConstraintState this
                                (valueC ++ bodyC ++ [ Constraint this bodyT ])
                        )
                        Env.freshTVar
                        (generate body (Env.extend name valueT env))
                )
                (generate value env)

        Spy exp_ tag ->
            State.map
                (\(ConstraintState typ cons) ->
                    ConstraintState typ (cons ++ [ Constraint (Type.TVar tag) typ ])
                )
                (generate exp_ env)
