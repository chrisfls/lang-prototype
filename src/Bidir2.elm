module Bidir2 exposing
    ( Exp, var, lam, app, ann, toString
    , Type, toStringT
    , State, empty
    , check
    )

{-|

@docs Exp, var, lam, app, ann, toString
@docs Type, toStringT
@docs State, empty
@docs check

-}

import Dict exposing (Dict)


type alias Name =
    String


type Type
    = TVar Argm Int
    | TArr Type Type


type alias Argm =
    Bool


type Exp
    = Var Name
    | Lam Name (Exp -> Exp)
    | App Exp Exp
    | Ann Type Exp


var : Name -> Exp
var =
    Var


lam : Name -> (Exp -> Exp) -> Exp
lam =
    Lam


app : Exp -> Exp -> Exp
app =
    App


ann : Type -> Exp -> Exp
ann =
    Ann


type State
    = State Env


type alias Env =
    { count : Int
    , args : Int
    , free : List Int
    , env : Dict Int Type
    , arg : Dict Int Type
    }


type alias Error =
    String


empty : State
empty =
    State (Env 0 0 [] Dict.empty Dict.empty)


get : Int -> Bool -> State -> Maybe Type
get at argm (State { env, arg }) =
    if argm then
        Dict.get at arg

    else
        Dict.get at env


insertType : Int -> Type -> State -> State
insertType at typ (State ({ env } as state)) =
    State { state | env = Dict.insert at typ env }


insertArgm : Int -> Type -> State -> State
insertArgm at typ (State ({ arg } as state)) =
    State { state | arg = Dict.insert at typ arg }


newTVar : State -> ( Type, State )
newTVar (State ({ count, free } as state)) =
    case free of
        count_ :: free_ ->
            ( TVar False count_, State { state | free = free_ } )

        _ ->
            ( TVar False count, State { state | count = count + 1 } )


newArgmTVar : State -> ( Type, State )
newArgmTVar (State ({ args } as state)) =
    ( TVar True args, State { state | args = args + 1 } )


check : Exp -> State -> Result Error ( Type, State )
check exp state =
    case infer exp state of
        Ok ( typ, state_ ) ->
            Ok ( typ, state_ )

        err ->
            err


infer : Exp -> State -> Result Error ( Type, State )
infer exp state =
    case exp of
        Var _ ->
            Debug.todo "TODO: unbound var error (proper error message)"

        Lam name body ->
            let
                ( argmT, newState ) =
                    newArgmTVar state
            in
            case infer (body (Ann argmT (Var name))) newState of
                Ok ( bodyT, finalState ) ->
                    Ok ( TArr (unify argmT finalState) (unify bodyT finalState), finalState )

                err ->
                    err

        App func argm ->
            case infer argm state of
                Ok ( argmT, state_ ) ->
                    case infer func state_ of
                        Ok ( funcT, _ ) ->
                            apply funcT argmT state_

                        err ->
                            err

                err ->
                    err

        Ann typ _ ->
            -- TODO: check if typ and exp are really compatible
            Ok ( typ, state )


apply : Type -> Type -> State -> Result Error ( Type, State )
apply funcT argmT state =
    case funcT of
        TVar argm index ->
            case get index argm state of
                Just withT ->
                    contraintWith withT argmT state

                Nothing ->
                    constraint index argmT state

        withT ->
            contraintWith withT argmT state


constraint : Int -> Type -> State -> Result error ( Type, State )
constraint index argmT state =
    let
        ( tvar, state_ ) =
            newTVar state
    in
    Ok ( tvar, insertArgm index (TArr argmT tvar) state_ )


contraintWith : Type -> Type -> State -> Result String ( Type, State )
contraintWith withT someT state =
    case withT of
        TArr funcT bodyT ->
            case someT of
                TVar _ index ->
                    let
                        state_ =
                            insertType index funcT state
                    in
                    Ok ( unify bodyT state_, state_ )

                _ ->
                    Err "Can't constrain someT to withT (TODO: try or elaborate)"

        _ ->
            Err "Can't constrain (TODO: try or elaborate)"


unify : Type -> State -> Type
unify thisT state =
    case thisT of
        TVar argm index ->
            case get index argm state of
                Just someT ->
                    if thisT == someT then
                        someT

                    else
                        unify someT state

                Nothing ->
                    thisT

        TArr l r ->
            TArr (unify l state) (unify r state)



--- visualization stuff


toString : Exp -> String
toString exp =
    case exp of
        Var name ->
            name

        Lam name body ->
            "(" ++ name ++ " -> " ++ toString (body (Var name)) ++ ")"

        App func argm ->
            "(" ++ toString func ++ " " ++ toString argm ++ ")"

        Ann typ (Lam name body) ->
            "(" ++ name ++ " [" ++ toStringT typ ++ "] -> " ++ toString (body (Var name)) ++ ")"

        Ann typ exp_ ->
            "[" ++ toStringT typ ++ "]" ++ toString exp_


toStringT : Type -> String
toStringT typ =
    case typ of
        TVar _ index ->
            let
                argm =
                    max 0 (index - 12)
            in
            case List.head (List.drop (modBy length index) vars) of
                Just a ->
                    if argm > 0 then
                        a ++ String.fromInt argm

                    else
                        a

                Nothing ->
                    "unk" ++ String.fromInt index

        TArr ((TArr _ _) as f) t ->
            "[" ++ toStringT f ++ "] -> " ++ toStringT t

        TArr ((TVar _ _) as f) t ->
            toStringT f ++ " -> " ++ toStringT t


vars : List String
vars =
    String.split "" "abcdefghijkl"


length : Int
length =
    List.length vars
