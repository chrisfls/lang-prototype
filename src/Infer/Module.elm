module Infer.Module exposing (..)

import IR.Linearity as Linearity
import IR.Module exposing (Module(..), ModuleExpr(..))
import IR.Spec as Spec exposing (Spec)
import Infer.Apply as Apply exposing (apply)
import Infer.Model as Model exposing (Model)
import Infer.ModuleBody as ModuleBody


type Return
    = Return Spec Model
    | Throw String


infer : Module -> Model -> Return
infer modl model =
    case modl of
        Param name _ body ->
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

        Let name expr body ->
            case inferExpr expr model of
                Return spec unwrapModel ->
                    Model.insertModule name (Model.unwrapSpec spec unwrapModel) model
                        |> infer body

                throw ->
                    throw

        ModuleBody body ->
            case ModuleBody.infer body model of
                ModuleBody.Return members moduleModel ->
                    Return (Spec.Struct members) moduleModel

                ModuleBody.Throw err ->
                    Throw err


inferExpr : ModuleExpr -> Model -> Return
inferExpr expr model =
    case expr of
        Variable name ->
            case Model.getModule name model of
                Just spec ->
                    Return spec model

                Nothing ->
                    Throw <| "module '" ++ name ++ "' not found"

        Apply function argument ->
            case inferExpr function model of
                Return functionSpec functionModel ->
                    case inferExpr argument functionModel of
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

        _ ->
            Debug.todo ""
