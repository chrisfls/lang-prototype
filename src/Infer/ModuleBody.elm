module Infer.ModuleBody exposing (Return(..), infer)

import IR.Members as Members
import IR.Module exposing (ModuleBody(..), Privacy(..))
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
        CloseModule ->
            Return members model

        DefSpec privacy name annotation nextBody ->
            let
                ( convertedSpec, unwrapModel ) =
                    convert annotation model

                spec =
                    Model.unwrapSpec convertedSpec unwrapModel

                nextModel =
                    Model.insertSpec name spec model
            in
            case privacy of
                Public ->
                    inferHelp (Members.insertSpec name spec members) nextBody nextModel

                Private ->
                    inferHelp members nextBody nextModel

        DefExpr privacy name expr nextBody ->
            case Expr.infer expr model of
                Expr.Return inferedSpec unwrapModel ->
                    let
                        spec =
                            Model.unwrapSpec inferedSpec unwrapModel

                        nextModel =
                            Model.insertExpr name False spec model
                    in
                    case privacy of
                        Public ->
                            inferHelp (Members.insertExpr name spec members) nextBody nextModel

                        Private ->
                            inferHelp members nextBody nextModel

                Expr.Throw err ->
                    Throw err
