module Lang.ConstraintGen exposing (..)

import Dict
import Lang.Monad exposing (..)
import Lang.Scheme exposing (Environment, freshTypevar, generalize, instantiate)
import Lang.Syntax.Expr exposing (Expr(..))
import Lang.Syntax.Type exposing (Type(..))


type alias Constraint =
    ( Type, Type )


generateConstraints : Expr -> Environment -> Monad ( Type, List Constraint )
generateConstraints exp environment =
    case exp of
        Var name ->
            variable name environment
                |> map (\x -> ( x, [] ))

        Lit t ->
            pure ( t, [] )

        App function argument ->
            map3
                (\this ( f, fc ) ( a, ac ) ->
                    ( this
                    , fc ++ ac ++ [ ( f, TArr a this ) ]
                    )
                )
                freshTypevar
                (generateConstraints function environment)
                (generateConstraints argument environment)

        Lam argument body ->
            freshTypevar
                |> andThen
                    (\argType ->
                        generateConstraints (body (Var argument)) (extend argument argType environment)
                            |> map
                                (\( bodyType, bodyCons ) ->
                                    ( TArr argType bodyType, bodyCons )
                                )
                    )

        Let name value body ->
            generateConstraints value environment
                |> andThen
                    (\( valueT, valueC ) ->
                        map2
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
                |> map
                    (\( typ, constraints ) ->
                        ( typ, constraints ++ [ ( TVar tag, typ ) ] )
                    )


variable : String -> Environment -> Monad Type
variable name env =
    Dict.get name env
        |> Result.fromMaybe ("variable " ++ name ++ " not found")
        |> fromResult
        |> andThen instantiate


extendGeneralized : String -> Type -> Environment -> Environment
extendGeneralized name t environment =
    Dict.insert name (generalize t environment) environment


extend : String -> Type -> Environment -> Environment
extend name t environment =
    Dict.insert name ( [], t ) environment
