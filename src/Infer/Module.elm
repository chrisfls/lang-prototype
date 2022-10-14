module Infer.Module exposing (Return(..), infer)

import IR.Linearity as Linearity
import IR.Module exposing (Module(..))
import IR.Spec as Spec exposing (Spec)
import Infer.Apply as Apply exposing (apply)
import Infer.Compare exposing (compareAnnotation)
import Infer.Model as Model exposing (Model)


type Return
    = Return Spec Model
    | Throw String


infer : Module -> Model -> Return
infer expr model =
    case expr of
        Variable name ->
            case Model.getModule name model of
                Just spec ->
                    Return spec model

                Nothing ->
                    Throw <| "module '" ++ name ++ "' not found"

        Lambda name body ->
            let
                ( address, freeAddressModel ) =
                    Model.nextFreeSpecAddress model

                argumentSpec =
                    Spec.Reference False address

                argumentModel =
                    Model.insertModule name argumentSpec freeAddressModel
            in
            case infer body argumentModel of
                Return bodySpec bodyModel ->
                    Return
                        (Spec.Arrow Linearity.Varying argumentSpec bodySpec)
                        (Model.removeModule name bodyModel)

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

        Annotation annotation subModule ->
            case infer subModule model of
                Return spec nextModel ->
                    case compareAnnotation annotation spec of
                        Just err ->
                            Throw err

                        Nothing ->
                            -- TODO: instead adopt original spec
                            Return spec nextModel

                err ->
                    err

        IfEquals _ _ _ _ ->
            -- TODO: assert that then and else have the same type
            Debug.todo "IfEquals"

        Module body ->
            Debug.todo "Module"
