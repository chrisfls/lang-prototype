module Infer.Expr exposing (Return(..), infer)

import IR.Expr exposing (Expr(..))
import IR.Linearity as Linearity
import IR.Spec as Spec exposing (Spec)
import Infer.Apply as Apply exposing (apply)
import Infer.Compare exposing (compareAnnotation)
import Infer.Convert exposing (convert)
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
                    if defaultLinearity == Linearity.Varying && Model.hasAnyLinearExpr model then
                        Linearity.Closure

                    else
                        defaultLinearity

                isLinear =
                    linearity == Linearity.Linear

                ( address, freeAddressModel ) =
                    Model.nextFreeSpecAddress model

                argumentSpec =
                    Spec.Reference isLinear address

                argumentModel =
                    Model.insertExpr name isLinear argumentSpec freeAddressModel
            in
            case infer body argumentModel of
                Return bodySpec bodyModel ->
                    Return
                        (Spec.Arrow linearity argumentSpec bodySpec)
                        (Model.removeExpr name isLinear bodyModel)

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
                Return spec _ ->
                    case compareAnnotation annotation spec of
                        Just err ->
                            Throw err

                        Nothing ->
                            let
                                ( convertedSpec, nextModel ) =
                                    convert annotation model
                            in
                            Return convertedSpec nextModel

                err ->
                    err
