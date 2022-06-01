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
    = State Int Env


type alias Env =
    Dict Int Type


type alias Error =
    String


empty : State
empty =
    State 0 Dict.empty


get : Int -> State -> Maybe Type
get key (State _ env) =
    Dict.get key env


insert : Int -> Type -> State -> State
insert key typ (State i env) =
    State i (Dict.insert key typ env)


newFreeIndex : State -> ( Type, State )
newFreeIndex (State i env) =
    ( TVar i, State (i + 1) env )


check : Exp -> State -> Result Error ( Type, State )
check exp state =
    case infer exp state of
        Ok ( typ, finalState ) ->
            Ok ( unify typ finalState, finalState )

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
                    newFreeIndex state
            in
            case infer (body (Ann argmT (Var name))) newState of
                Ok ( bodyT, finalState ) ->
                    Ok ( TArr argmT bodyT, finalState )

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
        TVar i ->
            case get i state of
                Just withT ->
                    contraintWith withT argmT state

                Nothing ->
                    constraint i argmT state

        withT ->
            contraintWith withT argmT state


constraint : Int -> Type -> State -> Result error ( Type, State )
constraint i argmT state =
    let
        ( tvar, finalState ) =
            newFreeIndex state
    in
    Ok ( tvar, insert i (TArr argmT tvar) finalState )


contraintWith : Type -> Type -> State -> Result String ( Type, State )
contraintWith withT someT state =
    case withT of
        TArr funcT bodyT ->
            case someT of
                TVar i ->
                    Ok ( bodyT, insert i funcT state )

                _ ->
                    Err "Can't constrain someT to withT (TODO: try or elaborate)"

        _ ->
            Err "Can't constrain (TODO: try or elaborate)"


unify : Type -> State -> Type
unify thisT state =
    case thisT of
        TVar i ->
            case get i state of
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
        TVar i ->
            let
                argm =
                    max 0 (i - 12)
            in
            case List.head (List.drop (modBy length i) vars) of
                Just a ->
                    if argm > 0 then
                        a ++ String.fromInt argm

                    else
                        a

                Nothing ->
                    "unk" ++ String.fromInt i

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
