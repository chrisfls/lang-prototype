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
insert index typeT state =
    let
        (State { env }) =
            state
    in
    case getLastTVar typeT env of
        (TVar _) as someT ->
            insertHelp index someT state

        TArr left right ->
            insertHelp index
                (TArr (getLastTVar left env) (getLastTVar right env))
                state


nextTVar : State -> ( Type, State )
nextTVar state =
    let
        ( index, state_ ) =
            nextTVarHelp state
    in
    ( TVar index, state_ )


get : Int -> State -> Maybe Type
get index state =
    let
        (State { env }) =
            state
    in
    getHelp index env


unwrap : Type -> State -> Type
unwrap typeT state =
    case typeT of
        TVar i ->
            case get i state of
                Just someT ->
                    if typeT == someT then
                        typeT

                    else
                        unwrap someT state

                Nothing ->
                    typeT

        TArr l r ->
            TArr (unwrap l state) (unwrap r state)



-- internals


insertHelp : Int -> Type -> State -> State
insertHelp index typeT state =
    let
        (State internals) =
            state
    in
    State { internals | env = Dict.insert index typeT internals.env }


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
        (Just typeT) as justT ->
            case typeT of
                TVar index_ ->
                    case getHelp index_ env of
                        (Just _) as justT_ ->
                            justT_

                        Nothing ->
                            justT

                _ ->
                    justT

        nothing ->
            nothing


getLastTVar : Type -> Env -> Type
getLastTVar typeT env =
    case typeT of
        TVar index ->
            getLastTVarHelp index typeT env

        _ ->
            typeT


getLastTVarHelp : Int -> Type -> Env -> Type
getLastTVarHelp index prevT env =
    case Dict.get index env of
        Just typeT ->
            case typeT of
                TVar index_ ->
                    getLastTVarHelp index_ typeT env

                _ ->
                    prevT

        Nothing ->
            prevT
