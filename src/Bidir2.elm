module Bidir2 exposing
    ( Exp(..), toString
    , Type(..), toStringT
    , State, empty
    , check
    )

{-|

@docs Exp, toString
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


type State
    = State Int Int Env


type alias Env =
    Dict ( Int, Int ) Type


type alias Error =
    String


empty : State
empty =
    State 0 0 Dict.empty


key : Anon -> Int -> ( Int, Int )
key anon index =
    if anon then
        ( 1, index )

    else
        ( 0, index )


get : Anon -> Int -> State -> Maybe Type
get anon index (State _ _ env) =
    Dict.get (key anon index) env


insert : Anon -> Int -> Type -> State -> State
insert anon index thisT (State count0 count1 env) =
    State count0 count1 (Dict.insert (key anon index) thisT env)


insertAnon : Int -> Type -> State -> State
insertAnon =
    insert True


newTVar : State -> ( Type, State )
newTVar (State count0 count1 env) =
    ( TVar False count0, State (count0 + 1) count1 env )


newAnonTVar : State -> ( Type, State )
newAnonTVar (State count0 count1 env) =
    ( TVar True count1, State count0 (count1 + 1) env )


check : Exp -> State -> Result Error ( Type, State )
check exp state =
    case exp of
        Var _ ->
            Debug.todo "TODO: unbound var error (proper error message)"

        Lam name body ->
            let
                ( argmT, newState1 ) =
                    newAnonTVar state
            in
            case check (body (Ann argmT (Var name))) newState1 of
                Ok ( bodyT, newState2 ) ->
                    let
                        ( bodyT_, finalState ) =
                            expose bodyT newState2
                    in
                    Ok ( TArr (unify argmT finalState) bodyT_, finalState )

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
        TVar anon index ->
            case get anon index state of
                Just withT ->
                    contraintWith withT argmT state

                Nothing ->
                    constraint index argmT state

        withT ->
            contraintWith withT argmT state


constraint : Int -> Type -> State -> Result error ( Type, State )
constraint index argmT state =
    let
        ( argmT_, newState ) =
            expose argmT state

        ( bodyT, finalState ) =
            newAnonTVar newState
    in
    Ok ( bodyT, insertAnon index (TArr argmT_ bodyT) finalState )


contraintWith : Type -> Type -> State -> Result String ( Type, State )
contraintWith withT thisT state =
    -- TODO: test this function
    case withT of
        TArr funcT bodyT ->
            case thisT of
                TVar anon index ->
                    let
                        state_ =
                            insert anon index funcT state
                    in
                    Ok ( unify bodyT state_, state_ )

                _ ->
                    Err "Can't constrain someT to withT (TODO: try or elaborate)"

        _ ->
            Err "Can't constrain shit (TODO: try or elaborate)"


expose : Type -> State -> ( Type, State )
expose thisT state =
    case thisT of
        TVar True argmI ->
            let
                ( someT, finalState ) =
                    newTVar state
            in
            ( someT, insertAnon argmI someT finalState )

        TVar False _ ->
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
        TVar anon index ->
            case get anon index state of
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
        TVar True index ->
            -- only here to help with debug
            String.fromInt index

        TVar False index ->
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
