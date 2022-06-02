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
    = TVar Anon Int
    | TArr Type Type


type alias Anon =
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
    { viewIndex : Int
    , viewEnv : Dict Int Type
    , anonIndex : Int
    , anonEnv : Dict Int Type
    }


type alias Error =
    String


empty : State
empty =
    State (Env 0 Dict.empty 0 Dict.empty)


get : Int -> Bool -> State -> Maybe Type
get index anon (State { viewEnv, anonEnv }) =
    if anon then
        Dict.get index anonEnv

    else
        Dict.get index viewEnv


insert : Int -> Anon -> Type -> State -> State
insert index anon thisT (State ({ viewEnv, anonEnv } as state)) =
    if anon then
        State { state | anonEnv = Dict.insert index thisT anonEnv }

    else
        State { state | viewEnv = Dict.insert index thisT viewEnv }


newTVar : State -> ( Type, State )
newTVar (State ({ viewIndex } as state)) =
    ( TVar False viewIndex, State { state | viewIndex = viewIndex + 1 } )


newAnonTVar : State -> ( Type, State )
newAnonTVar (State ({ anonIndex } as state)) =
    ( TVar True anonIndex, State { state | anonIndex = anonIndex + 1 } )


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
                    newAnonTVar state
            in
            case infer (body (Ann argmT (Var name))) newState of
                Ok ( bodyT, finalState ) ->
                    Ok ( TArr (unify argmT finalState) (unify bodyT finalState), finalState )

                err ->
                    err

        App func argm ->
            case infer argm state of
                Ok ( argmT, newState ) ->
                    case infer func newState of
                        Ok ( funcT, finalState ) ->
                            apply funcT argmT finalState

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
        TVar anon index ->
            case get index anon state of
                Just withT ->
                    contraintWith withT argmT state

                Nothing ->
                    constraint index anon argmT state

        withT ->
            contraintWith withT argmT state


constraint : Int -> Bool -> Type -> State -> Result error ( Type, State )
constraint index anon argmT state =
    let
        ( tvar, state_ ) =
            newTVar state
    in
    Ok ( tvar, insert index anon (TArr argmT tvar) state_ )


contraintWith : Type -> Type -> State -> Result String ( Type, State )
contraintWith withT thisT state =
    case withT of
        TArr funcT bodyT ->
            case thisT of
                TVar anon index ->
                    let
                        state_ =
                            insert index anon funcT state
                    in
                    Ok ( unify bodyT state_, state_ )

                _ ->
                    Err "Can't constrain someT to withT (TODO: try or elaborate)"

        _ ->
            Err "Can't constrain (TODO: try or elaborate)"


unify : Type -> State -> Type
unify thisT state =
    case thisT of
        TVar anon index ->
            case get index anon state of
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

        Ann thisT (Lam name body) ->
            "(" ++ name ++ " [" ++ toStringT thisT ++ "] -> " ++ toString (body (Var name)) ++ ")"

        Ann thisT exp_ ->
            "[" ++ toStringT thisT ++ "]" ++ toString exp_


toStringT : Type -> String
toStringT thisT =
    case thisT of
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
