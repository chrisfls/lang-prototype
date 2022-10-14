module Infer.ModuleBody exposing (Return(..), infer)

import IR.Members as Members
import IR.Module exposing (ModuleBody(..), Privacy(..))
import IR.Spec exposing (Members)
import Infer.Expr as Expr
import Infer.Model exposing (Model)


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

        DefSpec _ _ _ _ ->
            -- TODO: convert annotation into spec
            Debug.todo ""

        DefExpr privacy name expr nextBody ->
            case Expr.infer expr model of
                Expr.Return spec nextModel ->
                    case privacy of
                        Public ->
                            inferHelp (Members.insertExpr name spec members) nextBody nextModel

                        Private ->
                            inferHelp members nextBody model

                Expr.Throw err ->
                    Throw err
