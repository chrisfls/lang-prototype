module Bidir3 exposing (..)

import Dict exposing (Dict)


type alias Name =
    String


type Typ
    = TVar Int
    | TArr Typ Typ


type Exp
    = Var Name
    | Lam Name (Exp -> Exp)
    | App Exp Exp
    | Ann Typ Exp


var : Name -> Exp
var =
    Var


lam : Name -> (Exp -> Exp) -> Exp
lam =
    Lam


app : Exp -> Exp -> Exp
app =
    App


ann : Typ -> Exp -> Exp
ann =
    Ann


type State
    = State Int Env


type alias Env =
    Dict Int Typ


type alias Error =
    String


empty : State
empty =
    State 0 Dict.empty


get : Int -> State -> Maybe Typ
get key (State _ env) =
    Dict.get key env


insert : Int -> Typ -> State -> State
insert key typ (State idx env) =
    State idx (Dict.insert key typ env)


newFreeIndex : State -> ( Int, State )
newFreeIndex (State idx env) =
    ( idx, State (idx + 1) env )


check : Exp -> State -> Result Error ( Exp, Typ, State )
check exp state =
    let
        () =
            ()

        -- _ = Debug.log "exp" exp
    in
    case exp of
        Var _ ->
            Debug.todo "WTF1"

        Lam name body ->
            let
                ( idx, state1 ) =
                    newFreeIndex state

                tvar =
                    TVar idx
            in
            case check (body (Ann tvar (Var name))) state1 of
                Ok ( _, bodyT, state_ ) ->
                    let
                        typ =
                            TArr (unify tvar state_) (unify bodyT state_)
                    in
                    ( Ann typ exp
                    , typ
                    , state_
                    )
                        |> Ok

                Err e ->
                    Err e

        App func argm ->
            case check argm state of
                Ok ( _, argmT, state1 ) ->
                    case check func state1 of
                        Ok ( _, funcT, state2 ) ->
                            case applyTyp funcT argmT state2 of
                                Ok ( bodyT, state3 ) ->
                                    ( exp
                                    , unify bodyT state3
                                    , state3
                                    )
                                        |> Ok

                                Err e ->
                                    Err e

                        Err e ->
                            Err e

                Err e ->
                    Err e

        Ann typ exp_ ->
            ( exp_
            , typ
            , state
            )
                |> Ok


applyTyp : Typ -> Typ -> State -> Result Error ( Typ, State )
applyTyp funcT argmT state =
    case funcT of
        TVar idx ->
            case get idx state of
                Just typ ->
                    case typ of
                        TArr funcT_ bodyT ->
                            case argmT of
                                TVar idx_ ->
                                    ( bodyT
                                    , insert idx_ funcT_ state
                                    )
                                        |> Ok

                                _ ->
                                    Err "Deal with type error later1"

                        _ ->
                            Err "Deal with type error later2"

                Nothing ->
                    let
                        ( idx__, state_ ) =
                            newFreeIndex state

                        tvar =
                            TVar idx__
                    in
                    ( tvar
                    , insert idx (TArr argmT tvar) state_
                    )
                        |> Ok

        TArr l r ->
            Err ("can't apply type " ++ toStringTyp l ++ " + " ++ toStringTyp r)


unify : Typ -> State -> Typ
unify typ state =
    case typ of
        TVar idx ->
            case get idx state of
                Just typ_ ->
                    if typ == typ_ then
                        typ_

                    else
                        unify typ_ state

                Nothing ->
                    typ

        TArr l r ->
            TArr (unify l state) (unify r state)


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
            "(" ++ name ++ " [" ++ toStringTyp typ ++ "] -> " ++ toString (body (Var name)) ++ ")"

        Ann typ exp_ ->
            "[" ++ toStringTyp typ ++ "]" ++ toString exp_


toStringTyp : Typ -> String
toStringTyp typ =
    case typ of
        TVar i ->
            toStringVar i

        TArr ((TArr _ _) as f) t ->
            "[" ++ toStringTyp f ++ "] -> " ++ toStringTyp t

        TArr ((TVar _) as f) t ->
            toStringTyp f ++ " -> " ++ toStringTyp t


toStringVar : Int -> String
toStringVar arg =
    let
        arg_ =
            max 0 (arg - 12)
    in
    case List.head (List.drop (modBy 12 arg) vars) of
        Just a ->
            if arg_ > 0 then
                a ++ String.fromInt arg_

            else
                a

        Nothing ->
            "unk" ++ String.fromInt arg


vars : List String
vars =
    String.split "" "abcdefghijkl"


length : Int
length =
    List.length vars
