module Infer.Expr exposing (Return(..), infer)

import Dict exposing (Dict)
import IR.Expr exposing (Expr(..))
import IR.Spec as Spec exposing (Spec)
import Infer.Apply as Apply exposing (apply)
import Infer.Compare exposing (compareAnnotation)
import Infer.Model as Model exposing (Model)


type Return
    = Return Spec Model
    | Throw String


infer : Expr -> Model -> Return
infer expr model =
    case expr of
        Variable name ->
            case Model.getExpr name model of
                Just spec ->
                    if Model.isExprAvailable name model then
                        Return spec (Model.useExpr name model)

                    else
                        Throw <| "var '" ++ name ++ "' previously used"

                Nothing ->
                    Throw <| "var '" ++ name ++ "' not found"

        Lambda defaultLinearity name body ->
            let
                linearity =
                    if defaultLinearity == Spec.Varying && Model.hasAnyLinearExpr model then
                        Spec.Closure

                    else
                        defaultLinearity

                isLinear =
                    linearity == Spec.Linear

                ( address, freeAddressModel ) =
                    Model.nextFreeSpecAddress model

                argumentSpec =
                    Spec.Reference isLinear address

                argumentModel =
                    if isLinear then
                        Model.insertLinearExpr name argumentSpec freeAddressModel

                    else
                        Model.insertExpr name argumentSpec freeAddressModel
            in
            case infer body argumentModel of
                Return bodySpec bodyModel ->
                    Return
                        (Spec.Arrow linearity argumentSpec bodySpec)
                        (Model.removeExpr name bodyModel)

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

        Annotation annotation subExpr ->
            case infer subExpr model of
                Return spec nextModel ->
                    case compareAnnotation annotation spec of
                        Just err ->
                            Throw err

                        Nothing ->
                            -- TODO: instead adopt original spec
                            Return spec nextModel

                err ->
                    err


