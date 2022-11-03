module AST.Spec exposing (..)

import Parser exposing (..)


type Spec
    = Hole
    | Primitive Primitive
    | Apply Spec Spec
    | Variable Bool String


type Primitive
    = Unit
    | PrimBool
    | PrimChar
    | PrimInt
    | PrimFloat
    | PrimString



-- | Lambda Spec Spec
-- | Closure Spec Spec
-- | Tuple (List Spec)
-- | Record (Dict String Spec)
-- | ExtRecord (Dict String Spec)
-- | DotAccess Spec String
-- | Apply String (List Spec)
-- PRETTY PRINT
-- format : Spec -> Format.Text
-- format spec =
--     case spec of
--         Hole ->
--             Format.text "_"
--         Primitive prim ->
--             case prim of
--                 Unit ->
--                     Format.text "()"
--                 PrimBool ->
--                     Format.text "Bool"
--                 PrimChar ->
--                     Format.text "Char"
--                 PrimInt ->
--                     Format.text "Int"
--                 PrimFloat ->
--                     Format.text "Float"
--                 PrimString ->
--                     Format.text "String"
--         Variable True name ->
--             Format.text ("*" ++ name)
--         Variable False name ->
--             Format.text name
--         Apply left right ->
--             Debug.todo ""
-- let
--     leftStr =
--         toStringHelp column False indent left
--     partialLength =
--         column + String.length leftStr + 1
-- in
-- if partialLength < 80 then
--     let
--         rightStr =
--             toStringHelp column False indent right
--         totalLength =
--             partialLength + String.length rightStr
--     in
--     if totalLength < 80 then
--         leftStr ++ " " ++ rightStr
--     else
--         Debug.todo ""
-- else
--     Debug.todo ""
-- Apply left right ->
--     -- toStringApply left right lines indent
-- toStringApply left right lines indent state =
--     push "(" state
--         |> toStringHelp left lines indent
--         |> toStringHelp right lines indent
--         |> push ")"
-- push : String -> ToStringState -> ToStringState
-- push string state =
--     { state | head = state.head ++ string }
-- pushElement : Bool -> Int -> String -> ToStringState -> ToStringState
-- pushElement lines indent string state =
--     { head = String.repeat indent "  " ++ string, tail = state.head :: state.tail }
-- -- length : ToStringState -> Int
-- -- length state =
-- --     state.(List.foldl (String.length >> (+)) 0 state.tail)
-- toStringCommit : ToStringState -> List String
-- toStringCommit { head, tail } =
--     head :: tail
-- -- PARSER


parser : Parser Spec
parser =
    oneOf
        [ succeed Hole
            |. keyword "_"
        , succeed (Primitive PrimBool)
            |. keyword "Bool"
        , succeed (Primitive PrimChar)
            |. keyword "Char"
        , succeed (Primitive PrimInt)
            |. keyword "Int"
        , succeed (Primitive PrimFloat)
            |. keyword "Float"
        , succeed (Primitive PrimString)
            |. keyword "String"
        ]
