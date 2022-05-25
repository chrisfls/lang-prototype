module Lang.Infer.Constraint exposing (..)

import Basics.Extra exposing (flip)
import Lang.Canonical.Expr exposing (Expr(..))
import Lang.Canonical.Type.Internal exposing (Type(..))
import Lang.Infer.Error.Internal exposing (Error)
import Lang.Infer.Env as Env exposing (Env)
import Lang.Infer.State as State exposing (State)


type alias Constraint =
    ( Type, Type )


type alias Constraints =
    List Constraint


type alias GeneratorState =
    State Error ( Type, Constraints ) Int


mergeAppConstraints : Type -> ( Type, Constraints ) -> ( Type, Constraints ) -> ( Type, Constraints )
mergeAppConstraints typ ( func, funcConstraint ) ( argm, argConstraint ) =
    ( typ
    , funcConstraint ++ argConstraint ++ [ ( func, TArr argm typ ) ]
    )


aaa : a -> ( a, List b )
aaa x =
    ( x, [] )


bbb : Type -> Type -> Type
bbb =
    flip TArr


generate : Expr -> Env -> GeneratorState
generate expr env =
    case expr of
        Var name ->
            State.map aaa (Env.variable name env)

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
                        |> State.map (Tuple.mapFirst (bbb argmT))
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
                (\( typ, cons ) -> ( typ, cons ++ [ ( TVar tag, typ ) ] ))
                (generate exp_ env)
