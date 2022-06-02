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
    = TVar Int
    | TArr Type Type


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
    = State Int (List Int) Env


type alias Env =
    Dict Int Type


type alias Error =
    String


empty : State
empty =
    State 0 [] Dict.empty


get : Int -> State -> Maybe Type
get at (State _ _ env) =
    Dict.get at env


insert : Int -> Type -> State -> State
insert at typ (State count free env) =
    State count free (Dict.insert at typ env)


newTVar : State -> ( Type, State )
newTVar =
    newTVarI >> Tuple.mapFirst TVar 

newTVarI : State -> ( Int, State )
newTVarI (State count free env) =
    case free of
        count_ :: free_ ->
            ( count_, State count free_ env )

        _ ->
            ( count, State (count + 1) free env )



freeTVar : Int -> State -> State
freeTVar index (State count free env) =
    State count (index :: free) (Dict.remove index env)


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
                ( index, newState ) =
                    newTVarI state

                argmT =
                    TVar index
            in
            case infer (body (Ann argmT (Var name))) newState of
                Ok ( bodyT, finalState ) ->
                    Ok ( TArr (unify argmT finalState) (unify bodyT finalState), freeTVar index finalState )

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
        TVar index ->
            case get index state of
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
    Ok ( tvar, insert index (TArr argmT tvar) state_ )


contraintWith : Type -> Type -> State -> Result String ( Type, State )
contraintWith withT someT state =
    case withT of
        TArr funcT bodyT ->
            case someT of
                TVar index ->
                    let
                        state_ =
                            insert index funcT state
                    in
                    Ok ( unify bodyT state_, freeTVar index state_ )

                _ ->
                    Err "Can't constrain someT to withT (TODO: try or elaborate)"

        _ ->
            Err "Can't constrain (TODO: try or elaborate)"


unify : Type -> State -> Type
unify thisT state =
    case thisT of
        TVar index ->
            case get index state of
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
        TVar index ->
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

        TArr ((TVar _) as f) t ->
            toStringT f ++ " -> " ++ toStringT t


vars : List String
vars =
    String.split "" "abcdefghijkl"


length : Int
length =
    List.length vars
