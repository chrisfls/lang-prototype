module Lang.Infer.Constraint exposing (..)

import Lang.Canonical.Expr exposing (Expr(..))
import Lang.Canonical.Type.Internal as Type exposing (Type)
import Lang.Infer.Env as Env exposing (Env)
import Lang.Infer.Error.Internal exposing (Error)
import Lang.Infer.State as State exposing (State)


type alias Constraint =
    ( Type, Type )


type alias Constraints =
    List Constraint


type alias GenerateState =
    ( Type, Constraints )


singleton : Type -> GenerateState
singleton x =
    ( x, [] )


mergeAppConstraints : Type -> GenerateState -> GenerateState -> GenerateState
mergeAppConstraints typ ( func, funcConstraint ) ( argm, argConstraint ) =
    ( typ
    , funcConstraint ++ argConstraint ++ [ ( func, Type.TArr argm typ ) ]
    )


generate : Expr -> Env -> State Error GenerateState Int
generate expr env =
    case expr of
        Var name ->
            State.map singleton (Env.variable name env)

        Lit t ->
            State.empty ( t, [] )

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
                        |> State.map (Tuple.mapFirst (\a -> Type.TArr a argmT))
                )
                Env.freshTVar

        Let name value body ->
            State.andThen
                (\( valueT, valueC ) ->
                    State.map2
                        (\this ( bodyT, bodyC ) ->
                            ( this
                            , valueC ++ bodyC ++ [ ( this, bodyT ) ]
                            )
                        )
                        Env.freshTVar
                        (generate body (Env.extend name valueT env))
                )
                (generate value env)

        Spy exp_ tag ->
            State.map
                (\( typ, cons ) -> ( typ, cons ++ [ ( Type.TVar tag, typ ) ] ))
                (generate exp_ env)
