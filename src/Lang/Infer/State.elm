module Lang.Infer.State exposing (State(..), empty, get, insert, nextTVar, unify)

import Dict exposing (Dict)
import Lang.Canonical.Type exposing (Type(..))


type State
    = State Int Env


type alias Env =
    Dict Int Type


empty : State
empty =
    State 0 Dict.empty


insert : Int -> Type -> State -> State
insert index thisT state =
    case thisT of
        TVar index_ ->
            case get index_ state of
                Just someT ->
                    insert index someT state

                Nothing ->
                    insertHelp index thisT state

        TArr left right ->
            insertHelp index
                (TArr (getLastTVar left state) (getLastTVar right state))
                state


nextTVar : State -> ( Type, State )
nextTVar state =
    let
        ( index, state_ ) =
            nextTVarHelp state
    in
    ( TVar index, state_ )


get : Int -> State -> Maybe Type
get index (State _ env) =
    getHelp index env


unify : Type -> State -> State -> ( Type, State )
unify thisT original substitute =
    let
        ( someT, state ) =
            unifyHelp thisT (Unify Dict.empty original substitute)
    in
    ( someT, state.substitute )



-- internals


insertHelp : Int -> Type -> State -> State
insertHelp index thisT (State count env) =
    State count (Dict.insert index thisT env)


nextTVarHelp : State -> ( Int, State )
nextTVarHelp (State count env) =
    ( count, State (count + 1) env )


getHelp : Int -> Env -> Maybe Type
getHelp index env =
    case Dict.get index env of
        (Just (TVar index_)) as maybeT ->
            if index_ == index then
                maybeT
            else
                withMaybeDefault maybeT (getHelp index_ env)

        default ->
            default


getLastTVar : Type -> State -> Type
getLastTVar thisT (State _ env) =
    case thisT of
        TVar index ->
            Maybe.withDefault thisT (getLastTVarHelp index env)

        default ->
            default


getLastTVarHelp : Int -> Env -> Maybe Type
getLastTVarHelp index env =
    case Dict.get index env of
        (Just (TVar index_)) as maybeT ->
            if index == index_ then
                maybeT
            else
                withMaybeDefault maybeT (getLastTVarHelp index_ env)

        default ->
            default


type alias Unify =
    { remaps : Env, original : State, substitute : State }


unifyHelp : Type -> Unify -> ( Type, Unify )
unifyHelp thisT state =
    case thisT of
        TVar index ->
            case Dict.get index state.remaps of
                Just remap ->
                    ( remap, state )

                Nothing ->
                    let
                        ( someT, state_ ) =
                            case get index state.original of
                                Just thisT_ ->
                                    unifyHelp thisT_ state

                                Nothing ->
                                    let
                                        ( tvar, substitute_ ) =
                                            nextTVar state.substitute
                                    in
                                    ( tvar, { state | substitute = substitute_ } )
                    in
                    ( someT, { state_ | remaps = Dict.insert index someT state_.remaps })

        TArr func argm ->
            let
                ( func_, newState ) =
                    unifyHelp func state

                ( argm_, finalState ) =
                    unifyHelp argm newState
            in
            ( TArr func_ argm_, finalState )


withMaybeDefault : Maybe x -> Maybe x -> Maybe x
withMaybeDefault default override =
    case override of
        Just _ ->
            override

        Nothing ->
            default
