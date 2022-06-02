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
    = TAnon Int
    | TVar Int
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
    = State Int Int Env


type alias Env =
    Dict ( Int, Int ) Type


type alias Error =
    String


empty : State
empty =
    State 0 0 Dict.empty


get : Int -> State -> Maybe Type
get index (State _ _ env) =
    Dict.get ( 0, index ) env


insert : Int -> Type -> State -> State
insert index thisT (State count0 count1 env) =
    State count0 count1 (Dict.insert ( 0, index ) thisT env)


newTVar : State -> ( Type, State )
newTVar (State count0 count1 env) =
    ( TVar count0, State (count0 + 1) count1 env )


getAnon : Int -> State -> Maybe Type
getAnon index (State _ _ env) =
    Dict.get ( 1, index ) env


insertAnon : Int -> Type -> State -> State
insertAnon index thisT (State count0 count1 env) =
    State count0 count1 (Dict.insert ( 1, index ) thisT env)


newTAnon : State -> ( Type, State )
newTAnon (State count0 count1 env) =
    ( TAnon count1, State count0 (count1 + 1) env )


check : Exp -> State -> Result Error ( Type, State )
check exp state =
    case exp of
        Var _ ->
            Debug.todo "TODO: unbound var error (proper error message)"

        Lam name body ->
            let
                ( anonT, newState1 ) =
                    newTAnon state
            in
            case check (body (Ann anonT (Var name))) newState1 of
                Ok ( bodyT, newState2 ) ->
                    let
                        ( bodyT_, finalState ) =
                            expose bodyT newState2
                    in
                    Ok ( TArr (unify anonT finalState) (unify bodyT_ finalState), finalState )

                err ->
                    err

        App func argm ->
            case check argm state of
                Ok ( argmT, newState ) ->
                    case check func newState of
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
        TAnon index ->
            case get index state of
                Just withT ->
                    contraintWith withT argmT state

                Nothing ->
                    constraint index True argmT state

        TVar index ->
            case get index state of
                Just withT ->
                    contraintWith withT argmT state

                Nothing ->
                    constraint index False argmT state

        withT ->
            contraintWith withT argmT state


constraint : Int -> Bool -> Type -> State -> Result error ( Type, State )
constraint index anon argmT state =
    let
        ( argmT_, newState0 ) =
            expose argmT state

        ( bodyT, newState1 ) =
            newTAnon newState0

        finalState =
            if anon then
                insertAnon index (TArr argmT_ bodyT) newState1

            else
                insert index (TArr argmT_ bodyT) newState1
    in
    Ok ( bodyT, finalState )


contraintWith : Type -> Type -> State -> Result String ( Type, State )
contraintWith withT thisT state =
    -- TODO: check if this whole function is needed
    let
        _ =
            Debug.log "caraio" withT
    in
    case withT of
        TArr funcT bodyT ->
            case thisT of
                TAnon index ->
                    -- TODO: check if this is needed
                    let
                        state_ =
                            insertAnon index funcT state
                    in
                    Ok ( unify bodyT state_, state_ )

                TVar index ->
                    let
                        state_ =
                            insert index funcT state
                    in
                    Ok ( unify bodyT state_, state_ )

                _ ->
                    Err "Can't constrain someT to withT (TODO: try or elaborate)"

        _ ->
            Err "Can't constrain shit (TODO: try or elaborate)"


expose : Type -> State -> ( Type, State )
expose thisT state =
    case thisT of
        TAnon argmI ->
            let
                ( someT, finalState ) =
                    newTVar state
            in
            ( someT, insertAnon argmI someT finalState )

        TVar _ ->
            ( thisT, state )

        TArr left right ->
            let
                ( leftT, newState ) =
                    expose left state

                ( rightT, finalState ) =
                    expose right newState
            in
            ( TArr leftT rightT, finalState )


unify : Type -> State -> Type
unify thisT state =
    case thisT of
        TAnon index ->
            case getAnon index state of
                Just someT ->
                    unify someT state

                Nothing ->
                    thisT

        TVar index ->
            case get index state of
                Just someT ->
                    case someT of
                        TVar _ ->
                            thisT

                        _ ->
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
        TAnon index ->
            String.fromInt index

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

        TArr f t ->
            toStringT f ++ " -> " ++ toStringT t


vars : List String
vars =
    String.split "" "abcdefghijkl"


length : Int
length =
    List.length vars
