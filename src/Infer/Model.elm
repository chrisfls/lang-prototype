module Infer.Model exposing (..)

import Dict exposing (Dict)
import IR.Spec exposing (Address, Spec(..))
import IntDict exposing (IntDict)
import Set exposing (Set)


type alias Model =
    { graph : Graph
    , count : Int
    , scope : Dict String Spec
    , linear : Set String
    , state : Dict String State
    }


type alias Graph =
    IntDict Spec


type State
    = FreshNew
    | Required
    | Borrowed
    | Disposed


empty : Model
empty =
    { graph = IntDict.empty
    , count = 0
    , scope = Dict.empty
    , linear = Set.empty

    -- TODO: maybe optimize this into multiple sets
    , state = Dict.empty
    }


insertAtAddress : Address -> Spec -> Model -> Model
insertAtAddress address spec state =
    { state | graph = IntDict.insert address spec state.graph }


insertAtName : String -> Spec -> Model -> Model
insertAtName name spec state =
    { state | scope = Dict.insert name spec state.scope, state = Dict.insert name FreshNew state.state }


setLinearName : String -> Model -> Model
setLinearName name state =
    { state | linear = Set.insert name state.linear }

hasLinearNames : Model -> Bool
hasLinearNames model =
    not (Set.isEmpty model.linear)

setUsedName : String -> Model -> Model
setUsedName name state =
    { state | state = Dict.insert name Required state.state }


setDisposedName : String -> Model -> Model
setDisposedName name state =
    { state | state = Dict.insert name Disposed state.state, linear = Set.remove name state.linear }


removeAtName : String -> Model -> Model
removeAtName name state =
    { state
    | scope = Dict.remove name state.scope
    , linear = Set.remove name state.linear
    , state = Dict.remove name state.state
    }


nextFreeAddress : Model -> ( Address, Model )
nextFreeAddress ({ count } as state) =
    ( count, { state | count = count + 1 } )


getAtAddress : Address -> Model -> Maybe Spec
getAtAddress index state =
    getHelp index state.graph


listFreshNames : Model -> List String
listFreshNames model =
    Dict.toList model.state
        |> List.filterMap
            (\( name, state ) ->
                if state == FreshNew then
                    Just name

                else
                    Nothing
            )


getAtName : String -> Model -> Maybe Spec
getAtName name state =
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


unwrap : Spec -> Model -> Spec
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

        Arrow name func argm ->
            Arrow name (unwrap func state) (unwrap argm state)

        Linear subSpec ->
            Linear (unwrap subSpec state)

        Unborrow name subSpec ->
            Unborrow name (unwrap subSpec state)



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
