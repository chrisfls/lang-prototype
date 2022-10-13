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
    , unused : Set String
    }


type alias Graph =
    IntDict Spec


empty : Model
empty =
    { graph = IntDict.empty
    , count = 0
    , scope = Dict.empty
    , linear = Set.empty
    , unused = Set.empty
    }


getAtAddress : Address -> Model -> Maybe Spec
getAtAddress index model =
    getHelp index model.graph


insertAtAddress : Address -> Spec -> Model -> Model
insertAtAddress address spec model =
    { model | graph = IntDict.insert address spec model.graph }


nextFreeAddress : Model -> ( Address, Model )
nextFreeAddress ({ count } as model) =
    ( count, { model | count = count + 1 } )


getAtName : String -> Model -> Maybe Spec
getAtName name model =
    Dict.get name model.scope


insertAtName : String -> Spec -> Model -> Model
insertAtName name spec model =
    { model
        | scope = Dict.insert name spec model.scope
        , unused = Set.insert name model.unused
    }


insertLinearAtName : String -> Spec -> Model -> Model
insertLinearAtName name spec model =
    { model
        | scope = Dict.insert name spec model.scope
        , linear = Set.insert name model.linear
        , unused = Set.insert name model.unused
    }


removeAtName : String -> Model -> Model
removeAtName name model =
    { model
        | scope = Dict.remove name model.scope
        , linear = Set.remove name model.linear
        , unused = Set.remove name model.unused
    }


unwrap : Spec -> Model -> Spec
unwrap spec model =
    case spec of
        Reference _ address ->
            case getAtAddress address model of
                Just nextSpec ->
                    if spec == nextSpec then
                        spec

                    else
                        unwrap nextSpec model

                Nothing ->
                    spec

        Arrow linearity func argm ->
            Arrow linearity (unwrap func model) (unwrap argm model)


isAvailable : String -> Model -> Bool
isAvailable name model =
    if Set.member name model.linear then
        Set.member name model.unused

    else
        True



-- LINEARITY STATE


hasLinearReferences : Model -> Bool
hasLinearReferences model =
    not (Set.isEmpty model.linear)



-- STATE CONTROL


setUsedName : String -> Model -> Model
setUsedName name model =
    { model | unused = Set.remove name model.unused }



-- internals


getHelp : Address -> Graph -> Maybe Spec
getHelp address graph =
    case IntDict.get address graph of
        (Just spec) as justSpec ->
            case spec of
                Reference _ nextAddress ->
                    case getHelp nextAddress graph of
                        (Just _) as justSpec_ ->
                            justSpec_

                        Nothing ->
                            justSpec

                _ ->
                    justSpec

        nothing ->
            nothing
