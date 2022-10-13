module Infer.Expr exposing (Return(..), infer)

import IR.Expr exposing (Expr(..))
import IR.Spec as Spec exposing (Spec)
import Infer.Apply as Apply exposing (apply)
import Infer.Model as Model exposing (Model)


type Return
    = Return Spec Model
    | Throw String


infer : Expr -> Model -> Return
infer expr model =
    case expr of
        Variable name ->
            case Model.getAtName name model of
                Just spec ->
                    if Model.isAvailable name model then
                        Return spec (Model.setUsedName name model)

                    else
                        Throw <| "var '" ++ name ++ "' previously used"

                Nothing ->
                    Throw <| "var '" ++ name ++ "' not found"

        Lambda defaultLinearity name body ->
            let
                linearity =
                    if defaultLinearity == Spec.Varying && Model.hasLinearReferences model then
                        Spec.Closure

                    else
                        defaultLinearity

                isLinear =
                    linearity == Spec.Linear

                ( address, freeAddressModel ) =
                    Model.nextFreeAddress model

                argumentSpec =
                    Spec.Reference isLinear address

                argumentModel =
                    if isLinear then
                        Model.insertLinearAtName name argumentSpec freeAddressModel

                    else
                        Model.insertAtName name argumentSpec freeAddressModel
            in
            case infer body argumentModel of
                Return bodySpec bodyModel ->
                    Return
                        (Spec.Arrow linearity argumentSpec bodySpec)
                        (Model.removeAtName name bodyModel)

                err ->
                    err

        Apply function argument ->
            case infer function model of
                Return functionSpec functionModel ->
                    case infer argument functionModel of
                        Return argumentSpec argumentModel ->
                            case apply functionSpec argumentSpec argumentModel of
                                Apply.Return applySpec applyModel ->
                                    Return applySpec applyModel

                                Apply.Throw msg ->
                                    Throw msg

                        err ->
                            err

                err ->
                    err

        Annotation _ _ ->
            -- TODO: typecheck
            Debug.todo "inferExpr Annotation"
