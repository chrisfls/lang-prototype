module Infer.Apply exposing (Return, apply)

import IR.Spec exposing (Spec(..))
import Infer.State as State exposing (State)


type alias Return =
    Result String { spec : Spec, state : State }


apply : Spec -> Spec -> State -> Return
apply functionSpec argumentSpec state =
    case functionSpec of
        -- TODO: deal with linearity
        Reference address ->
            case State.getByAddress address state of
                Just specAtAddress ->
                    contrainWith specAtAddress argumentSpec state

                Nothing ->
                    constrain address argumentSpec state

        _ ->
            contrainWith functionSpec argumentSpec state


constrain : Int -> Spec -> State -> Return
constrain index argumentSpec state =
    let
        ( returnAddress, nextState ) =
            State.nextFreeAddress state

        returnReference =
            Reference returnAddress
    in
    -- NOTE: having unamed arrows will hurt ability to infer frees...
    Ok
        { spec = returnReference
        , state = State.insertAtAddress index (Arrow Nothing argumentSpec returnReference) nextState
        }


contrainWith : Spec -> Spec -> State -> Return
contrainWith functionSpec argumentSpec state =
    case functionSpec of
        Arrow _ innerFunctionArgumentSpec innerFunctionReturnSpec ->
            constrainFunctionWith innerFunctionArgumentSpec innerFunctionReturnSpec argumentSpec state

        spec ->
            Debug.todo <| "contrainWith " ++ Debug.toString spec


constrainFunctionWith : Spec -> Spec -> Spec -> State -> Return
constrainFunctionWith argumentSpec returnSpec appliedSpec state =
    case appliedSpec of
        Reference address ->
            Ok
                { spec = returnSpec
                , state = State.insertAtAddress address argumentSpec state
                }

        spec ->
            Debug.todo <| "constrainFunctionWith " ++ Debug.toString spec
