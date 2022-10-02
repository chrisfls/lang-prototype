module Infer.Apply exposing (apply)

import IR.Spec exposing (Spec(..))
import Infer.Return exposing (Return(..))
import Infer.State as State exposing (State)


apply : Spec -> Spec -> State -> Return
apply functionSpec argumentSpec state =
    case functionSpec of
        Reference address ->
            case State.getByAddress address state of
                Just specAtAddress ->
                    contrainWith specAtAddress argumentSpec state

                Nothing ->
                    constrain address argumentSpec state

        _ ->
            contrainWith functionSpec argumentSpec state


constrain : Int -> Spec -> State -> Return
constrain index spec state =
    let
        ( returnAddress, nextState ) =
            State.nextFreeAddress state

        returnReference =
            Reference returnAddress
    in
    -- NOTE: having unamed arrows will hurt ability to infer frees...
    Return returnReference (State.insertAtAddress index (Arrow Nothing spec returnReference) nextState)


contrainWith : Spec -> Spec -> State -> Return
contrainWith functionSpec argumentSpec state =
    case functionSpec of
        Arrow _ innerFunctionArgumentSpec innerFunctionReturnSpec ->
            constrainFunctionWith innerFunctionArgumentSpec innerFunctionReturnSpec argumentSpec state

        _ ->
            Throw "TODO: try or elaborate why you can't constrain tB to tA when tA is not an arrow"


constrainFunctionWith : Spec -> Spec -> Spec -> State -> Return
constrainFunctionWith argumentSpec returnSpec appliedSpec state =
    case appliedSpec of
        Reference address ->
            State.insertAtAddress address argumentSpec state
                |> Return returnSpec

        _ ->
            Throw "TODO: try or elaborate why you can't constrain tB to tA"
