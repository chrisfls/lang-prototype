module Infer.Expr exposing (Return(..), infer)

import IR.Expr exposing (Expr(..))
import IR.Spec as Spec exposing (Spec)
import IR.SpecExpr as SpecExpr exposing (SpecExpr)
import Infer.Apply exposing (apply)
import Infer.Model as Model exposing (Model, nextFreeAddress)
import Set exposing (Set)


type Return
    = Return SpecExpr Model
    | Throw String


infer : Expr -> Model -> Return
infer expr state =
    case inferExpr expr False state of
        Return specExpr nextState ->
            Return specExpr nextState

        throw ->
            throw


inferExpr : Expr -> Bool -> Model -> Return
inferExpr expr mock model =
    case expr of
        Variable name ->
            case Model.getAtName name model of
                Just spec ->
                    Return (SpecExpr.Variable spec name) (Model.setUsedName name model)

                Nothing ->
                    Throw <| "var '" ++ name ++ "' not found"

        Lambda linear name body ->
            inferLambdaClosure mock (linear || Model.hasLinearNames model) linear name body model

        Closure linear name body ->
            inferLambdaClosure mock True linear name body model

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


inferLambdaClosure mock selfLinear linear name body model =
    -- TODO: simplify this shite
    let
        ( address, freeAddressModel ) =
            Model.nextFreeAddress model

        argumentSpec =
            Spec.Reference linear address

        linearModel =
            if linear then
                Model.setLinearName name freeAddressModel

            else
                freeAddressModel

        argumentModel =
            Model.insertAtName name argumentSpec linearModel
    in
    case inferExpr body True argumentModel of
        Return mockInfer mockModel ->
            if mock then
                let
                    returnSpec =
                        SpecExpr.toSpec mockInfer

                    justName =
                        Just name

                    specExpr =
                        if selfLinear then
                            SpecExpr.Closure (Spec.LinearArrow linear justName argumentSpec returnSpec) linear name mockInfer

                        else
                            SpecExpr.Lambda (Spec.Arrow justName argumentSpec returnSpec) name mockInfer
                in
                Return specExpr (Model.removeAtName name mockModel)

            else
                let
                    freshNames =
                        Model.listFreshNames mockModel

                    disposedModel =
                        List.foldl Model.setDisposedName argumentModel freshNames
                in
                case inferExpr body False disposedModel of
                    Return bodyInfer bodyModel ->
                        let
                            ( returnSpec, bodySpecExpr ) =
                                unborrow freshNames bodyInfer

                            justName =
                                Just name

                            specExpr =
                                if selfLinear then
                                    SpecExpr.Closure (Spec.LinearArrow linear justName argumentSpec returnSpec) linear name bodySpecExpr

                                else
                                    SpecExpr.Lambda (Spec.Arrow justName argumentSpec returnSpec) name bodySpecExpr
                        in
                        Return specExpr (Model.removeAtName name bodyModel)

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
