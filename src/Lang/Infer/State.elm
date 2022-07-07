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
        (Var _) as someT ->
            insertHelp index someT state

        Arr left right ->
            insertHelp index
                (Arr (getLastTVar left env) (getLastTVar right env))
                state


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
unwrap typeT state =
    case typeT of
        Var i ->
            case get i state of
                Just someT ->
                    if typeT == someT then
                        typeT

                    else
                        unwrap someT state

                Nothing ->
                    typeT

        Arr l r ->
            Arr (unwrap l state) (unwrap r state)



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
                Var index_ ->
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
        Var index ->
            getLastTVarHelp index typeT env

        _ ->
            typeT


getLastTVarHelp : Int -> Type -> Env -> Type
getLastTVarHelp index prevT env =
    case Dict.get index env of
        Just typeT ->
            case typeT of
                Var index_ ->
                    getLastTVarHelp index_ typeT env

                _ ->
                    prevT

        Nothing ->
            prevT
