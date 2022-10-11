module Infer.Expr exposing (Return(..), infer)

import IR.Expr exposing (Expr(..))
import IR.Spec as Spec exposing (Spec)
import Infer.Apply as Apply exposing (apply)
import Infer.Model as Model exposing (Model)


type Return
    = Return Spec Model
    | Throw String


infer : Expr -> Model -> Return
infer expr state =
    inferExpr False expr state


inferExpr : Bool -> Expr -> Model -> Return
inferExpr mock expr model =
    case expr of
        Variable name ->
            -- TODO: check if name is available
            case Model.getAtName name model of
                Just ( spec, state ) ->
                    case state of
                        Model.Available ->
                            Return spec (Model.setUsedName name model)

                        Model.Borrowed ->
                            Throw <| "var '" ++ name ++ "' previously borrowed"

                        Model.Disposed ->
                            Throw <| "var '" ++ name ++ "' already disposed"

                Nothing ->
                    Throw <| "var '" ++ name ++ "' not found"

        Lambda closure isLinear name body ->
            let
                isClosure =
                    closure || isLinear || Model.hasLinearReferences model

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
            case inferExpr True body argumentModel of
                Return mockSpec mockModel ->
                    if mock then
                        Return
                            (Spec.Arrow isClosure isLinear (Just name) argumentSpec mockSpec)
                            (Model.removeAtName name mockModel)

                    else
                        let
                            freshNames =
                                Model.listFreshNames mockModel

                            disposedModel =
                                List.foldl Model.setDisposedName (Model.setUsedName name argumentModel) freshNames
                        in
                        case inferExpr False body disposedModel of
                            Return bodySpec bodyModel ->
                                Return
                                    (Spec.Arrow isClosure isLinear (Just name) argumentSpec <|
                                        List.foldl Spec.Unborrow bodySpec freshNames
                                    )
                                    (Model.removeAtName name bodyModel)

                            err ->
                                err

                err ->
                    err

        Apply function argument ->
            case infer function model of
                Return functionSpec functionModel ->
                    case infer argument functionModel of
                        Return argumentSpec argumentModel ->
                            -- TODO: when applying a lambda that returns a Free type, add the values back to the unborrowed list
                            case apply functionSpec argumentSpec argumentModel of
                                Apply.Return applySpec applyModel ->
                                    Return applySpec applyModel

                                Apply.Throw msg ->
                                    Throw msg

                        err ->
                            err

                err ->
                    err

        Unborrow name subExpr ->
            case infer subExpr <| Model.setDisposedName name <| Model.setUsedName name model of
                Return bodySpec bodyModel ->
                    Return (Spec.Unborrow name bodySpec) bodyModel

                err ->
                    err

        Annotation _ _ ->
            -- TODO: typecheck
            Debug.todo "inferExpr Annotation"
