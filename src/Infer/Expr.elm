module Infer.Expr exposing (Return, infer)

import IR.Expr exposing (Expr(..))
import IR.Spec as Spec exposing (Spec)
import Infer.Apply exposing (apply)
import Infer.State as State exposing (State)
import Set exposing (Set)


type alias Return =
    Result String { expr : Expr, spec : Spec, state : State }


infer : Expr -> State -> Return
infer expr state =
    -- TODO: decide if it is worth to infer frees at the user language or here
    -- it is probably worth to do a free inference step before infering the rest of the expr
    case expr of
        Variable name ->
            case State.getByName name state of
                Just spec ->
                    Ok
                        { expr = expr
                        , spec = spec
                        , state = State.insertUsedName name state
                        }

                Nothing ->
                    Err "TODO: var not found error"

        Lambda linear name body ->
            let
                ( address, nextState1 ) =
                    State.nextFreeAddress state

                argumentSpec =
                    -- TODO: maybe mark reference as possibly linear
                    Spec.Reference address

                nextState2 =
                    State.insertAtName name argumentSpec nextState1
            in
            case infer body nextState2 of
                Ok bodyInfer ->
                    let
                        nextState3 =
                            State.removeAtName name bodyInfer.state

                        spec =
                            Spec.Arrow (Just name) argumentSpec bodyInfer.spec
                    in
                    Ok
                        { expr = Annotation True spec (Lambda linear name bodyInfer.expr)
                        , spec = spec
                        , state = nextState3
                        }

                err ->
                    err

        Apply function argument ->
            case infer function state of
                Ok functionInfer ->
                    case infer argument functionInfer.state of
                        Ok argumentInfer ->
                            -- TODO: when applying a lambda that returns a Free type, add the values back to the unborrowed list
                            case apply functionInfer.spec argumentInfer.spec argumentInfer.state of
                                Ok resultInfer ->
                                    Ok
                                        { expr = Annotation True resultInfer.spec expr
                                        , spec = resultInfer.spec
                                        , state = resultInfer.state
                                        }

                                Err msg ->
                                    Err msg

                        err ->
                            err

                err ->
                    err

        Free name subExpr ->
            case infer subExpr <| State.removeAtName name state of
                Ok argumentInfer ->
                    Ok
                        { expr = Free name argumentInfer.expr
                        , spec = argumentInfer.spec
                        , state = argumentInfer.state
                        }

                err ->
                    err

        Annotation True spec _ ->
            Ok
                { expr = expr
                , spec = spec
                , state = state
                }

        Annotation False _ _ ->
            -- TODO: typecheck
            Debug.todo "infer Annotation False"



-- wrapInFrees : List String -> Spec -> State -> ( Spec, State )
-- wrapInFrees list spec state =
--     case list of
--         name :: tail ->
--             wrapInFrees tail (Spec.Free name spec) (State.removeLinear name state)
--         [] ->
--             ( spec, state )
