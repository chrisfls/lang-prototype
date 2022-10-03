module Infer.InferExpr exposing (..)

import IR.Expr exposing (Expr(..))
import IR.Spec as Spec exposing (Spec)
import Infer.Apply exposing (apply)
import Infer.Return exposing (Return(..))
import Infer.State as State exposing (State)


infer : Expr -> State -> Return
infer expr state =
    case expr of
        Variable name ->
            case State.getByName name state of
                Just spec ->
                    Return spec <| State.insertBorrow name state

                Nothing ->
                    Debug.todo "undefined error"

        Closure name body ->
            -- TODO: optimize
            let
                ( address, nextState1 ) =
                    State.nextFreeAddress state

                argumentSpec =
                    Spec.Linear <| Spec.Reference address

                nextState2 =
                    State.insertAtName name argumentSpec nextState1
            in
            case infer body nextState2 of
                Return returnSpec nextState3 ->
                    let
                        lastState =
                            State.removeAtName name nextState3
                                |> State.removeBorrow name

                        inferedSpec =
                            Spec.Arrow (Just name) argumentSpec (wrapInFrees (State.getFrees nextState3) returnSpec)
                    in
                    Return (Spec.Linear inferedSpec) lastState

                throw ->
                    throw


        Lambda name body ->
            -- TODO: error if function is explicitly or implicitly non-linear when linear variables are available
            let
                ( address, nextState1 ) =
                    State.nextFreeAddress state

                argumentSpec =
                    Spec.Reference address

                nextState2 =
                    State.insertAtName name argumentSpec nextState1
            in
            case infer body nextState2 of
                Return returnSpec nextState3 ->
                    let
                        lastState =
                            State.removeAtName name nextState3
                                |> State.removeBorrow name

                        inferedSpec =
                            Spec.Arrow (Just name) argumentSpec (wrapInFrees (State.getFrees nextState3) returnSpec)
                    in
                    Return inferedSpec lastState

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

        Free name subExpr ->
            case infer subExpr <| State.removeBorrow name <| State.removeAtName name state of
                Return argumentSpec nextState ->
                    Return (Spec.Free name argumentSpec) nextState

                throw ->
                    throw

        Annotation True spec _ ->
            Return spec state

        Annotation False _ _ ->
            -- TODO: typecheck
            Debug.todo "a"


wrapInFrees : List String -> Spec -> Spec
wrapInFrees list spec =
    case list of
        name :: tail ->
            wrapInFrees tail (Spec.Free name spec)

        [] ->
            spec
