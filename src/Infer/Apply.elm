module Infer.Apply exposing (Return, apply)

import IR.Spec exposing (Spec(..))
import Infer.Model as State exposing (Model)


type alias Return =
    Result String { spec : Spec, state : Model }



-- TODO: when applying a free the reference should be marked as unborrowed


apply : Spec -> Spec -> Model -> Return
apply functionSpec argumentSpec state =
    case functionSpec of
        -- TODO: deal with linearity
        Reference _ address ->
            case State.getAtAddress address state of
                Just specAtAddress ->
                    contrainWith specAtAddress argumentSpec state

                Nothing ->
                    constrain address argumentSpec state

        _ ->
            contrainWith functionSpec argumentSpec state


constrain : Int -> Spec -> Model -> Return
constrain index argumentSpec state =
    let
        ( returnAddress, nextState ) =
            State.nextFreeAddress state

        returnReference =
            Reference False returnAddress
    in
    -- TODO: generate named args to help with linear argument inference
    -- TODO: apply linearity constraints
    Ok
        { spec = returnReference
        , state = State.insertAtAddress index (Arrow False False Nothing argumentSpec returnReference) nextState
        }


contrainWith : Spec -> Spec -> Model -> Return
contrainWith functionSpec argumentSpec state =
    case functionSpec of
        Arrow _ _ _ innerFunctionArgumentSpec innerFunctionReturnSpec ->
            constrainFunctionWith innerFunctionArgumentSpec innerFunctionReturnSpec argumentSpec state

        spec ->
            Debug.todo <| "contrainWith " ++ Debug.toString spec


constrainFunctionWith : Spec -> Spec -> Spec -> Model -> Return
constrainFunctionWith argumentSpec returnSpec appliedSpec state =
    case appliedSpec of
        Reference _ address ->
            Ok
                { spec = returnSpec
                , state = State.insertAtAddress address argumentSpec state
                }

        spec ->
            Debug.todo <| "constrainFunctionWith " ++ Debug.toString spec
