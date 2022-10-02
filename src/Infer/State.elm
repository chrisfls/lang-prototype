module Infer.State exposing (State, assignName, empty, getAtAddress, getByName, insertAtAddress, nextFreeAddress, unassignName, unwrap)

import Dict exposing (Dict)
import IR.Spec exposing (Address, Spec(..))
import IntDict exposing (IntDict)



-- TODO: enrich state with linear values available


type alias State =
    { graph : Graph
    , count : Int
    , scope : Scope
    }


type alias Graph =
    IntDict Spec



-- TODO: use IntDict


type alias Scope =
    Dict String Spec


empty : State
empty =
    State IntDict.empty 0 Dict.empty


insertAtAddress : Address -> Spec -> State -> State
insertAtAddress address spec state =
    { state | graph = IntDict.insert address spec state.graph }


assignName : String -> Spec -> State -> State
assignName name spec state =
    { state | scope = Dict.insert name spec state.scope }


unassignName : String -> State -> State
unassignName name state =
    { state | scope = Dict.remove name state.scope }


nextFreeAddress : State -> ( Address, State )
nextFreeAddress ({ count } as state) =
    ( count, { state | count = count + 1 } )


getAtAddress : Address -> State -> Maybe Spec
getAtAddress index state =
    getHelp index state.graph


getByName : String -> State -> Maybe Spec
getByName name state =
    -- TODO: maybe unwrap
    case Dict.get name state.scope of
        (Just spec) as justSpec ->
            case spec of
                Reference address ->
                    case getAtAddress address state of
                        (Just _) as justSpec_ ->
                            justSpec_

                        Nothing ->
                            justSpec

                _ ->
                    justSpec

        _ ->
            Nothing


unwrap : Spec -> State -> Spec
unwrap spec state =
    case spec of
        Reference address ->
            case getAtAddress address state of
                Just nextSpec ->
                    if spec == nextSpec then
                        spec

                    else
                        unwrap nextSpec state

                Nothing ->
                    spec

        Arrow func argm ->
            Arrow (unwrap func state) (unwrap argm state)

        Linear subSpec ->
            Linear (unwrap subSpec state)

        Free _ _ ->
            Debug.todo "UNWRAP FREE"



-- internals


getHelp : Address -> Graph -> Maybe Spec
getHelp address graph =
    case IntDict.get address graph of
        (Just spec) as justSpec ->
            case spec of
                Reference nextAddress ->
                    case getHelp nextAddress graph of
                        (Just _) as justSpec_ ->
                            justSpec_

                        Nothing ->
                            justSpec

                _ ->
                    justSpec

        nothing ->
            nothing
