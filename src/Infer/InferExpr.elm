module Infer.InferExpr exposing (..)

import IR.Expr exposing (Expr(..))
import IR.Spec as Spec
import Infer.Apply exposing (apply)
import Infer.Return exposing (Return(..))
import Infer.State as State exposing (State)


infer : Expr -> State -> Return
infer expr state =
    case expr of
        Variable name ->
            case State.getByName name state of
                Just spec ->
                    Return spec state

                Nothing ->
                    Debug.todo "undefined error"

        Lambda name body ->
            -- TODO: normal lambdas and
            -- TODO: when infering linear functions one must track
            --       unique var names available
            --
            --       if the function happens to be the last scope
            --       for an unborrowed var, it should be marked as unused
            let
                ( address, nextState1 ) =
                    State.nextFreeAddress state

                argumentSpec =
                    -- if linear then
                    --     Linear <| Reference address
                    -- else
                    Spec.Reference address

                nextState2 =
                    State.assignName name argumentSpec nextState1
            in
            case infer body nextState2 of
                Return returnSpec lastState ->
                    -- returned spec must be linear too
                    -- if value returned is not another linear function
                    -- mark which vars are freed here
                    State.unassignName name lastState
                        |> Return (Spec.Arrow argumentSpec returnSpec)

                throw ->
                    throw

        Apply function argument ->
            case infer function state of
                Return functionSpec nextState1 ->
                    case infer argument nextState1 of
                        Return argumentSpec nextState2 ->
                            -- TODO: when applying a lambda that returns a Free type, add the values back to the unborrowed list
                            apply functionSpec argumentSpec nextState2

                        throw ->
                            throw

                throw ->
                    throw

        Free _ _ ->
            -- TODO: typecheck
            Debug.todo "a"

        Annotation True spec _ ->
            Return spec state

        Annotation False _ _ ->
            -- TODO: typecheck
            Debug.todo "a"
