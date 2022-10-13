module Infer.Apply exposing (Return(..), apply)

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
            case Model.getAtAddress address model of
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
            Model.nextFreeAddress model

        returnReference =
            Reference False returnAddress
    in
    -- TODO: generate named args to help with linear argument inference
    -- TODO: apply linearity constraints
    Return returnReference (Model.insertAtAddress index (Arrow Spec.Varying Nothing argumentSpec returnReference) nextModel)


contrainWith : Spec -> Spec -> Model -> Return
contrainWith functionSpec argumentSpec model =
    case functionSpec of
        Arrow _ _ innerFunctionArgumentSpec innerFunctionReturnSpec ->
            constrainFunctionWith innerFunctionArgumentSpec innerFunctionReturnSpec argumentSpec model

        spec ->
            Debug.todo <| "contrainWith " ++ Debug.toString spec


constrainFunctionWith : Spec -> Spec -> Spec -> Model -> Return
constrainFunctionWith argumentSpec returnSpec appliedSpec model =
    case appliedSpec of
        Reference _ address ->
            -- (f a) when a is free = constrain a to f's argument
            Return returnSpec (Model.insertAtAddress address argumentSpec model)

        Arrow applyLinearity _ appliedArgumentSpec appliedReturnSpec ->
            -- (f a) when a and f are functions = constrain each argument to themselves
            case Model.unwrap argumentSpec model of
                Arrow argumentLinearity _ (Reference _ address) nestedReturnSpec ->
                    -- linearity is enforced at the function side, not parameter, parameter only checks for unborrows
                    let
                        _ =
                            Debug.log "argumentLinearity " argumentLinearity
                    in
                    constrainFunctionWith nestedReturnSpec returnSpec (unborrow appliedReturnSpec) <| Model.insertAtAddress address appliedArgumentSpec model

                spec ->
                    Debug.todo <| "constrainFunctionWith 1" ++ Debug.toString spec

        _ ->
            case Model.unwrap argumentSpec model of
                Reference _ address ->
                    -- (f a) when f's argument is free = constrain f's argument to a
                    Return returnSpec (Model.insertAtAddress address appliedSpec model)

                spec ->
                    Debug.todo <| "constrainFunctionWith 2" ++ Debug.toString spec

unborrow : Spec -> Spec
unborrow spec =
    case spec of
        Unborrow _ nextSpec ->
            nextSpec

        _ ->
            spec
