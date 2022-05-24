module Lang.Constraint exposing (..)

import Basics.Extra exposing (flip)
import Lang.Canonical.Expr exposing (Expr(..))
import Lang.Canonical.Type as Type exposing (Type(..))
import Lang.TypeEnv as TypeEnv exposing (TypeEnv)
import StateResult as StateResult exposing (StateResult)


type alias Constraint =
    ( Type, Type )


type alias Constraints =
    List Constraint


type alias GeneratorState =
    StateResult String ( Type, Constraints ) Int


mergeAppConstraints : Type -> ( Type, Constraints ) -> ( Type, Constraints ) -> ( Type, Constraints )
mergeAppConstraints typ ( func, funcConstraint ) ( argm, argConstraint ) =
    ( typ
    , funcConstraint ++ argConstraint ++ [ ( func, TArr argm typ ) ]
    )


freshTVar : StateResult error Type Int
freshTVar =
    StateResult.fromState Type.freshTVar


aaa : a -> ( a, List b )
aaa x =
    ( x, [] )


bbb : Type -> Type -> Type
bbb =
    flip TArr


generate : Expr -> TypeEnv -> GeneratorState
generate expr env =
    case expr of
        Var name ->
            StateResult.map aaa (TypeEnv.variable name env)

        Lit t ->
            StateResult.empty ( t, [] )

        App func argm ->
            StateResult.map3 mergeAppConstraints
                freshTVar
                (generate func env)
                (generate argm env)

        Lam argm body ->
            StateResult.andThen
                (\argmT ->
                    TypeEnv.extend argm argmT env
                        |> generate (body (Var argm))
                        |> StateResult.map (Tuple.mapFirst (bbb argmT))
                )
                freshTVar

        Let name value body ->
            StateResult.andThen
                (\( valueT, valueC ) ->
                    StateResult.map2
                        (\this ( bodyT, bodyC ) ->
                            ( this
                            , valueC ++ bodyC ++ [ ( this, bodyT ) ]
                            )
                        )
                        freshTVar
                        (generate body (TypeEnv.extend name valueT env))
                )
                (generate value env)

        Spy exp_ tag ->
            StateResult.map
                (\( typ, cons ) -> ( typ, cons ++ [ ( TVar tag, typ ) ] ))
                (generate exp_ env)
