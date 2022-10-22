module Infer.ModuleBody2 exposing (Return(..), infer)

import IR.Members as Members
import IR.Module2 exposing (ModuleBody(..))
import IR.Spec exposing (Members)
import Infer.Convert exposing (convert)
import Infer.Expr as Expr
import Infer.Model as Model exposing (Model)


type Return
    = Return Members Model
    | Throw String


infer : ModuleBody -> Model -> Return
infer =
    inferHelp Members.empty


inferHelp : Members -> ModuleBody -> Model -> Return
inferHelp members body model =
    case body of
        ReturnModule ->
            Return members model

        DefSpec name annotation nextBody ->
            let
                ( convertedSpec, unwrapModel ) =
                    convert annotation model

                spec =
                    Model.unwrapSpec convertedSpec unwrapModel

                nextModel =
                    Model.insertSpec name spec model
            in
            inferHelp (Members.insertSpec name spec members) nextBody nextModel

        DefExpr name expr nextBody ->
            case Expr.infer expr model of
                Expr.Return inferedSpec unwrapModel ->
                    let
                        spec =
                            Model.unwrapSpec inferedSpec unwrapModel

                        nextModel =
                            Model.insertExpr name False spec model
                    in
                    inferHelp (Members.insertExpr name spec members) nextBody nextModel

                Expr.Throw err ->
                    Throw err
