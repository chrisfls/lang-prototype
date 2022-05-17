module Lang.ConstraintGen exposing (..)

import Dict
import Lang.Monad as Monad
import Lang.Scheme exposing (Environment, freshTypevar, generalize, instantiate)
import Lang.Syntax.Expr exposing (Expr(..))
import Lang.Syntax.Type exposing (Type(..))
import State.Result as StateResult exposing (StateResult)

type alias Constraint =
    ( Type, Type )

app : Type -> (a, List (a, Type)) -> (Type, List (a, Type)) -> (Type, List (a, Type))
app this (f, fc) (a, ac) = 
    ( this
    , fc ++ ac ++ [ ( f, TArr a this ) ]
    )

--apply : (state -> ( value, state )) -> 
-- apply : (a -> StateResult String value Int ) -> StateResult String a Int -> StateResult String value Int
-- apply f s =
--     f s



generateConstraints2 : Expr -> Environment -> StateResult String ( Type, List Constraint ) Int
generateConstraints2 exp environment =
    case exp of
        Var name ->
            variable name environment
                |> StateResult.map (\x -> ( x, [] ))

        Lit t ->
            StateResult.empty (t , [])

        App function argument ->
            StateResult.empty app
                |> StateResult.andMap freshTypevar
                |> StateResult.andMap (generateConstraints function environment)
                |> StateResult.andMap (generateConstraints argument environment)

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
                    
generateConstraints : Expr -> Environment -> (Int -> ( Result String ( Type, List Constraint ), Int ))
generateConstraints exp environment =
    case exp of
        Var name ->
            variable name environment
                |> StateResult.map (\x -> ( x, [] ))

        Lit t ->
            StateResult.empty ( t, [] )

        App function argument ->
            StateResult.empty app
                |> StateResult.andMap freshTypevar
                |> StateResult.andMap (generateConstraints function environment)
                |> StateResult.andMap (generateConstraints argument environment)

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
