module Infer.Expr exposing (Return(..), toSpecExpr)

import IR.Expr exposing (Expr(..))
import IR.Spec as Spec exposing (Spec)
import IR.SpecExpr as SpecExpr exposing (SpecExpr)
import Infer.Apply exposing (apply)
import Infer.Model as Model exposing (Model, nextFreeAddress)
import Set exposing (Set)


type Return
    = Return SpecExpr Model
    | Throw String


toSpecExpr : Expr -> Model -> Return
toSpecExpr expr state =
    case infer expr False state of
        Return specExpr nextState ->
            Return specExpr nextState

        throw ->
            throw


infer : Expr -> Bool -> Model -> Return
infer expr mock model =
    -- TODO: there are two kinds of linearity,
    --       lambda linearity and parameter linearity
    --       every lambda with linear parameters are linear themselves
    --       but the other way is not always true
    case expr of
        Variable name ->
            case Model.getAtName name model of
                Just spec ->
                    Return (SpecExpr.Variable spec name) (Model.setUsedName name model)

                Nothing ->
                    Throw <| "var '" ++ name ++ "' not found"

        Lambda linear name body ->
            if mock then
                inferLambdaMock (linear || Model.hasLinearNames model) name body model

            else
                inferLambda (linear || Model.hasLinearNames model) name body model

        Closure linear name body ->
            Debug.todo "a"

        Apply function argument ->
            -- case inferExpr function model of
            --     Ok functionInfer ->
            --         case inferExpr argument functionInfer.state of
            --             Ok argumentInfer ->
            --                 -- TODO: when applying a lambda that returns a Free type, add the values back to the unborrowed list
            --                 case apply functionInfer.spec argumentInfer.spec argumentInfer.state of
            --                     Ok resultInfer ->
            --                         Ok
            --                             { expr = Annotation resultInfer.spec expr
            --                             , spec = resultInfer.spec
            --                             , state = resultInfer.state
            --                             }
            --                     Err msg ->
            --                         Err msg
            --             err ->
            --                 err
            --     err ->
            --         err
            Debug.todo "inferExpr Apply"

        Unborrow name subExpr ->
            -- case inferExpr subExpr <| Model.removeAtName name state of
            --     Ok argumentInfer ->
            --         -- Ok
            --         --     { expr = Unborrow name argumentInfer.expr
            --         --     , spec = argumentInfer.spec
            --         --     , state = argumentInfer.state
            --         --     }
            --         Debug.todo "a"
            --     err ->
            --         err
            Debug.todo "inferExpr Unborrow"

        Annotation _ _ ->
            -- TODO: typecheck
            Debug.todo "inferExpr Annotation"


inferLambdaMock isLinear name body model =
    let
        ( address, freeAddressModel ) =
            Model.nextFreeAddress model

        argumentSpec =
            Spec.Reference isLinear address

        linearModel =
            if isLinear then
                Model.setLinearName name freeAddressModel

            else
                freeAddressModel
    in
    case infer body True <| Model.insertAtName name argumentSpec linearModel of
        Return bodyInfer bodyModel ->
            let
                lambdaState =
                    Model.removeAtName name bodyModel

                lambdaSpec =
                    Spec.Arrow (Just name) argumentSpec (SpecExpr.toSpec bodyInfer)
            in
            Return (SpecExpr.Lambda lambdaSpec name bodyInfer) lambdaState

        err ->
            err


inferLambda isLinear name body model =
    let
        ( address, freeAddressModel ) =
            Model.nextFreeAddress model

        argumentSpec =
            Spec.Reference isLinear address

        linearModel =
            if isLinear then
                Model.setLinearName name freeAddressModel

            else
                freeAddressModel

        argumentModel =
            Model.insertAtName name argumentSpec linearModel
    in
    case infer body True argumentModel of
        Return _ tempModel ->
            let
                freshNames =
                    Model.listFreshNames tempModel

                disposedModel =
                    List.foldl Model.setDisposedName argumentModel freshNames
            in
            case infer body False disposedModel of
                Return bodyInfer bodyState ->
                    let
                        lambdaState =
                            Model.removeAtName name bodyState

                        ( returnSpec, bodySpecExpr ) =
                            unborrow freshNames bodyInfer

                        lambdaSpec =
                            Spec.Arrow (Just name) argumentSpec returnSpec
                    in
                    Return (SpecExpr.Lambda lambdaSpec name bodySpecExpr) lambdaState

                err ->
                    err

        err ->
            err


unborrow : List String -> SpecExpr -> ( Spec, SpecExpr )
unborrow list specExpr =
    unborrowHelp list (SpecExpr.toSpec specExpr) specExpr


unborrowHelp : List String -> Spec -> SpecExpr -> ( Spec, SpecExpr )
unborrowHelp list spec specExpr =
    case list of
        name :: tail ->
            let
                nextSpec =
                    Spec.Unborrow name (SpecExpr.toSpec specExpr)
            in
            unborrowHelp tail nextSpec (SpecExpr.Unborrow nextSpec name specExpr)

        [] ->
            ( spec, specExpr )
