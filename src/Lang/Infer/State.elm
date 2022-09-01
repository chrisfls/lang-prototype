module Lang.Infer.State exposing (State(..), empty, get, insert, nextTVar, unwrap)

import Dict exposing (Dict)
import Lang.Canonical.Type exposing (Type(..))


type State
    = State Internal


type alias Internal =
    { count : Int
    , env : Env
    }


type alias Env =
    Dict Int Type


empty : State
empty =
    State (Internal 0 Dict.empty)


insert : Int -> Type -> State -> State
insert index t state =
    let
        (State { env }) =
            state
    in
    case getLastTVar t env of
        (Var _) as t_ ->
            insertHelp index t_ state

        Arr left right ->
            insertHelp index
                (Arr (getLastTVar left env) (getLastTVar right env))
                state

        Tup types ->
            insertHelp index
                (Tup (List.map (\t_ -> getLastTVar t_ env) types))
                state

        Rec maybeE fields ->
            insertHelp index
                (Rec
                    (Maybe.map (\t_ -> getLastTVar t_ env) maybeE)
                    (Dict.map (\_ t_ -> getLastTVar t_ env) fields)
                )
                state

        (Bul _) as t_ ->
            insertHelp index t_ state


nextTVar : State -> ( Type, State )
nextTVar state =
    let
        ( index, state_ ) =
            nextTVarHelp state
    in
    ( Var index, state_ )


get : Int -> State -> Maybe Type
get index state =
    let
        (State { env }) =
            state
    in
    getHelp index env


unwrap : Type -> State -> Type
unwrap t state =
    case t of
        Var i ->
            case get i state of
                Just t_ ->
                    if t == t_ then
                        t

                    else
                        unwrap t_ state

                Nothing ->
                    t

        Arr l r ->
            Arr (unwrap l state) (unwrap r state)

        Tup xs ->
            Tup (List.map (\t_ -> unwrap t_ state) xs)

        Rec mx xs ->
            Rec (Maybe.map (\t_ -> unwrap t_ state) mx)
                (Dict.map (\_ t_ -> unwrap t_ state) xs)

        Bul _ ->
            t



-- internals


insertHelp : Int -> Type -> State -> State
insertHelp index t state =
    let
        (State internals) =
            state
    in
    State { internals | env = Dict.insert index t internals.env }


nextTVarHelp : State -> ( Int, State )
nextTVarHelp state =
    let
        (State internals) =
            state
    in
    ( internals.count, State { internals | count = internals.count + 1 } )


getHelp : Int -> Env -> Maybe Type
getHelp index env =
    case Dict.get index env of
        (Just t) as just ->
            case t of
                Var index_ ->
                    case getHelp index_ env of
                        (Just _) as just_ ->
                            just_

                        Nothing ->
                            just

                _ ->
                    just

        nothing ->
            nothing


getLastTVar : Type -> Env -> Type
getLastTVar t env =
    case t of
        Var index ->
            getLastTVarHelp index t env

        _ ->
            t


getLastTVarHelp : Int -> Type -> Env -> Type
getLastTVarHelp index t env =
    case Dict.get index env of
        Just t_ ->
            case t_ of
                Var index_ ->
                    getLastTVarHelp index_ t_ env

                _ ->
                    t

        Nothing ->
            t
