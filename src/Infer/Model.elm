module Infer.Model exposing
    ( Model
    , derefSpec
    , empty
    , getExpr
    , hasAnyLinearExpr
    , insertExpr
    , insertSpecPtr
    , isExprAvailable
    , nextFreeSpecAddress
    , removeExpr
    , unwrapSpec
    , useExpr
    )

import IR.Spec exposing (Address, Spec(..))
import Infer.Model.Bindings as Bindings exposing (Bindings)
import Infer.Model.Graph as Graph exposing (Graph)
import Infer.Model.Ownership as Ownership exposing (Ownership)
import IntDict exposing (IntDict)


type alias Model =
    { exprs : Bindings
    , specs : Bindings
    , graph : Graph
    , ownership : Ownership
    }


empty : Model
empty =
    { exprs = Bindings.empty
    , specs = Bindings.empty
    , graph = Graph.empty
    , ownership = Ownership.empty
    }


insertExpr : String -> Bool -> Spec -> Model -> Model
insertExpr name linear spec model =
    if linear then
        { model | exprs = Bindings.insert name spec model.exprs, ownership = Ownership.insert name model.ownership }

    else
        { model | exprs = Bindings.insert name spec model.exprs }


useExpr : String -> Model -> Model
useExpr name model =
    { model | ownership = Ownership.use name model.ownership }


removeExpr : String -> Bool -> Model -> Model
removeExpr name linear model =
    if linear then
        { model | exprs = Bindings.remove name model.exprs, ownership = Ownership.remove name model.ownership }

    else
        { model | exprs = Bindings.remove name model.exprs }


getExpr : String -> Model -> Maybe Spec
getExpr name { exprs } =
    Bindings.get name exprs


isExprAvailable : String -> Model -> Bool
isExprAvailable name { ownership } =
    Ownership.isAvailable name ownership


hasAnyLinearExpr : Model -> Bool
hasAnyLinearExpr { ownership } =
    Ownership.hasAny ownership


insertSpecPtr : Address -> Spec -> Model -> Model
insertSpecPtr address spec model =
    { model | graph = Graph.insert address spec model.graph }


derefSpec : Address -> Model -> Maybe Spec
derefSpec address { graph } =
    Graph.get address graph


nextFreeSpecAddress : Model -> ( Address, Model )
nextFreeSpecAddress model =
    let
        ( address, graph ) =
            Graph.nextFreeAddress model.graph
    in
    ( address, { model | graph = graph } )


unwrapSpec : Spec -> Model -> Spec
unwrapSpec spec { specs, graph } =
    unwrapSpecHelp spec specs (Graph.toIntDict graph)



-- internals


unwrapSpecHelp : Spec -> Bindings -> IntDict Spec -> Spec
unwrapSpecHelp spec specs store =
    case spec of
        Reference _ address ->
            case IntDict.get address store of
                Just nextSpec ->
                    if spec == nextSpec then
                        spec

                    else
                        unwrapSpecHelp nextSpec specs store

                Nothing ->
                    spec

        Arrow linearity func argm ->
            Arrow linearity (unwrapSpecHelp func specs store) (unwrapSpecHelp argm specs store)
