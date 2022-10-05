module Infer.InferExpr exposing (Return(..), infer)

import IR.Expr exposing (Expr(..))
import IR.Spec as Spec exposing (Spec)
import Infer.Apply exposing (apply)
import Infer.State as State exposing (State)


type Return
    = Return Expr Spec State
    | Throw String


infer : Expr -> State -> Return
infer expr state =
    inferHelp expr False state


inferHelp: Expr -> Bool -> State -> Return
inferHelp expr linear state =
    -- TODO: decide if it is worth to infer frees at the user language or here
    -- it is probably worth to do a free inference step before infering the rest of the expr
    case expr of
        Variable name ->
            -- case State.getByName name state of
            --     Just spec ->
            --         Return spec <| State.insertBorrow name state

            --     Nothing ->
            --         Debug.todo "undefined error"
            Debug.todo "infer Variable"

        Lambda maybeLinear name body ->
            -- case maybeLinear of
            --     Just True ->
            --         inferLambda True name body state
            --     Just False ->
            --         -- free every linear var before progressing
            --         inferLambda False name body state
            --     Nothing ->
            --         case inferLambda False name body state of
            --             Throw _ ->
            --                 inferLambda True name body state
            --             return ->
            --                 return
            Debug.todo "Lambda"

        Apply function argument ->
            -- case infer function state of
            --     Return _ functionSpec nextState1 ->
            --         case infer argument nextState1 of
            --             Return _ argumentSpec nextState2 ->
            --                 -- TODO: when applying a lambda that returns a Free type, add the values back to the unborrowed list
            --                 apply functionSpec argumentSpec nextState2

            --             throw ->
            --                 throw

            --     throw ->
            --         throw
            Debug.todo "infer Apply"

        Free name subExpr ->
            -- case infer subExpr <| State.removeAtName name state of
            --     Return _ argumentSpec nextState ->
            --         Return (Spec.Free name argumentSpec) nextState

            --     throw ->
            --         throw
            Debug.todo "infer Free"

        Annotation True spec _ ->
            -- Return spec state
            Debug.todo "infer Annotation True"

        Annotation False _ _ ->
            -- TODO: typecheck
            Debug.todo "infer Annotation False"



-- inferLambda : Bool -> String -> Expr -> State -> Return
-- inferLambda linear name body state =
--     let
--         ( address, nextState1 ) =
--             State.nextFreeAddress state
--         argumentSpec =
--             if linear then
--                 Spec.Linear <| Spec.Reference address
--             else
--                 Spec.Reference address
--         nextState2 =
--             State.insertAtName name argumentSpec nextState1
--                 |> State.insertLinear name
--     in
--     case inferHelp body linear nextState2 of
--         Return returnSpec nextState3 ->
--             let
--                 nextState4 =
--                     State.removeAtName name nextState3
--                 ( finalReturnSpec, lastState ) =
--                     wrapInFrees (State.getFrees nextState3) returnSpec nextState4
--                 inferedSpec =
--                     Spec.Arrow (Just name) argumentSpec finalReturnSpec
--             in
--             if linear then
--                 case Debug.log "INFERED" argumentSpec of
--                     _ ->
--                         Return (Spec.Linear inferedSpec) lastState
--             else
--                 Return inferedSpec lastState
--         throw ->
--             throw
-- wrapInFrees : List String -> Spec -> State -> ( Spec, State )
-- wrapInFrees list spec state =
--     case list of
--         name :: tail ->
--             wrapInFrees tail (Spec.Free name spec) (State.removeLinear name state)
--         [] ->
--             ( spec, state )
