module Infer.Apply exposing (Return(..), apply)

import IR.Expr as Expr
import IR.Spec as Spec exposing (Spec(..))
import Infer.Model as Model exposing (Model)


type Return
    = Return Spec Model
    | Throw String



-- TODO: when applying a free the reference should be marked as unborrowed


apply : Spec -> Spec -> Model -> Return
apply functionSpec argumentSpec model =
    case functionSpec of
        -- TODO: deal with linearity
        Reference _ address ->
            case Model.derefSpec address model of
                Just specAtAddress ->
                    contrainWith specAtAddress argumentSpec model

                Nothing ->
                    constrain address argumentSpec model

        _ ->
            contrainWith functionSpec argumentSpec model


constrain : Int -> Spec -> Model -> Return
constrain index argumentSpec model =
    let
        ( returnAddress, nextModel ) =
            Model.nextFreeSpecAddress model

        returnReference =
            Reference False returnAddress
    in
    -- TODO: generate named args to help with linear argument inference
    -- TODO: apply linearity constraints
    Return returnReference (Model.insertSpecPtr index (Arrow Spec.Varying argumentSpec returnReference) nextModel)


contrainWith : Spec -> Spec -> Model -> Return
contrainWith functionSpec argumentSpec model =
    case functionSpec of
        Arrow _ innerFunctionArgumentSpec innerFunctionReturnSpec ->
            constrainFunctionWith innerFunctionArgumentSpec innerFunctionReturnSpec argumentSpec model

        spec ->
            Debug.todo <| "contrainWith " ++ Debug.toString spec


constrainFunctionWith : Spec -> Spec -> Spec -> Model -> Return
constrainFunctionWith argumentSpec returnSpec appliedSpec model =
    case appliedSpec of
        Reference _ address ->
            -- (f a) when a is free = constrain a to f's argument
            Return returnSpec (Model.insertSpecPtr address argumentSpec model)

        Arrow applyLinearity appliedArgumentSpec appliedReturnSpec ->
            -- (f a) when a and f are functions = constrain each argument to themselves
            case Model.unwrapSpec argumentSpec model of
                Arrow argumentLinearity (Reference _ address) nestedReturnSpec ->
                    -- linearity is enforced at the function side, not parameter
                    if argumentLinearity /= Spec.Linear || applyLinearity == Spec.Linear then
                        -- TODO: check if appliedArgumentSpec is compatible with nestedArgumentSpec when it is not a Reference
                        constrainFunctionWith nestedReturnSpec returnSpec appliedReturnSpec <| Model.insertSpecPtr address appliedArgumentSpec model

                    else
                        Throw <| "Expected " ++ Debug.toString argumentLinearity ++ " but found " ++ Debug.toString applyLinearity

                spec ->
                    Debug.todo <| "constrainFunctionWith" ++ Debug.toString spec
