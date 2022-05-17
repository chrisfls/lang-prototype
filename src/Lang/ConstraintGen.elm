module Lang.ConstraintGen exposing (..)

import Dict
import Lang.Monad as Monad
import Lang.Scheme exposing (Environment, generalize, instantiate)
import Lang.Syntax.Expr exposing (Expr(..))
import Lang.Syntax.Type exposing (Type(..))
import State.Result as StateResult exposing (StateResult)


type alias Constraint =
    ( Type, Type )


freshInt : Int -> ( Result String Int, Int )
freshInt state =
    ( Ok state, state + 1 )


freshTypevar : Int -> ( Result String Type, Int )
freshTypevar =
    StateResult.map TVar freshInt


app : Type -> ( Type, List Constraint ) -> ( Type, List Constraint ) -> ( Type, List Constraint )
app typ ( func, funcConstraint ) ( argm, argConstraint ) =
    ( typ
    , funcConstraint ++ argConstraint ++ [ ( func, TArr argm typ ) ]
    )


generateConstraints : Expr -> Environment -> StateResult String ( Type, List Constraint ) Int
generateConstraints exp environment =
    case exp of
        Var name ->
            StateResult.map (\x -> ( x, [] )) (variable name environment)

        Lit t ->
            StateResult.empty ( t, [] )

        App function argument ->
            StateResult.map3 app
                freshTypevar
                (generateConstraints function environment)
                (generateConstraints argument environment)

        Lam argument body ->
            freshTypevar
                |> StateResult.andThen
                    (\argType ->
                        generateConstraints (body (Var argument)) (extend argument argType environment)
                            |> StateResult.map
                                (\( bodyType, bodyCons ) ->
                                    ( TArr argType bodyType, bodyCons )
                                )
                    )

        Let name value body ->
            generateConstraints value environment
                |> StateResult.andThen
                    (\( valueT, valueC ) ->
                        StateResult.map2
                            (\this ( bodyT, bodyC ) ->
                                ( this
                                , valueC ++ bodyC ++ [ ( this, bodyT ) ]
                                )
                            )
                            freshTypevar
                            (generateConstraints body (extend name valueT environment))
                    )

        Spy exp_ tag ->
            generateConstraints exp_ environment
                |> StateResult.map
                    (\( typ, constraints ) ->
                        ( typ, constraints ++ [ ( TVar tag, typ ) ] )
                    )


variable : String -> Environment -> (Int -> ( Result String Type, Int ))
variable name env =
    Dict.get name env
        |> Result.fromMaybe ("variable " ++ name ++ " not found")
        |> Monad.fromResult
        |> StateResult.andThen instantiate


extendGeneralized : String -> Type -> Environment -> Environment
extendGeneralized name t environment =
    Dict.insert name (generalize t environment) environment


extend : String -> Type -> Environment -> Environment
extend name t environment =
    Dict.insert name ( [], t ) environment
