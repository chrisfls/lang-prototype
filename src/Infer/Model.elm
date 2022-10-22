module Infer.Model exposing
    ( Model
    , derefSpec
    , empty
    , getExpr
    , getModule
    , hasAnyLinearExpr
    , insertExpr
    , insertModule
    , insertSpec
    , insertSpecPtr
    , isExprAvailable
    , nextFreeSpecAddress
    , removeExpr
    , removeModule
    , unwrapSpec
    , useExpr
    )

import IR.Bindings as Bindings exposing (Bindings)
import IR.Spec exposing (Spec(..))
import Infer.Model.Graph as Graph exposing (Graph)
import Infer.Model.Ownership as Ownership exposing (Ownership)
import IntDict exposing (IntDict)

-- TODO: store privacy info for bindings (insert public members here during Infer.Module)

type alias Model =
    { modules : Bindings
    , exprs : Bindings
    , specs : Bindings
    , graph : Graph
    , ownership : Ownership
    }


empty : Model
empty =
    { modules = Bindings.empty
    , exprs = Bindings.empty
    , specs = Bindings.empty
    , graph = Graph.empty
    , ownership = Ownership.empty
    }



-- Modules


insertModule : String -> Spec -> Model -> Model
insertModule name spec model =
    { model | modules = Bindings.insert name spec model.modules }


removeModule : String -> Model -> Model
removeModule name model =
    { model | modules = Bindings.remove name model.modules }


getModule : String -> Model -> Maybe Spec
getModule name { modules } =
    Bindings.get name modules



-- Expr


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



-- Spec


insertSpec : String -> Spec -> Model -> Model
insertSpec name spec model =
    { model | specs = Bindings.insert name spec model.specs }


insertSpecPtr : Int -> Spec -> Model -> Model
insertSpecPtr address spec model =
    { model | graph = Graph.insert address spec model.graph }


derefSpec : Int -> Model -> Maybe Spec
derefSpec address { graph } =
    Graph.get address graph


nextFreeSpecAddress : Model -> ( Int, Model )
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

        _ ->
            Debug.todo "unwrap Module / SpecAt"
