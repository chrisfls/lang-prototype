module AST.Spec.Test exposing (suite)

import AST exposing (..)
import AST.Spec as Spec exposing (Spec)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import IR.Annotation as Annotation exposing (Annotation)
import Parser
import Test exposing (..)


suite : Test
suite =
    skip <|
        describe "AST.Spec"
            []



-- generate "Hole" Spec.Hole "_"
--         -- , generate "Unit" (Spec.Primitive Spec.Unit) "()"
--         , generate "Bool" (Spec.Primitive Spec.PrimBool) "Bool"
--         , generate "Char" (Spec.Primitive Spec.PrimChar) "Char"
--         , generate "Int" (Spec.Primitive Spec.PrimInt) "Int"
--         , generate "Float" (Spec.Primitive Spec.PrimFloat) "Float"
--         , generate "String" (Spec.Primitive Spec.PrimString) "String"
--         , test "abxy" <|
--             \_ ->
--                 Spec.toString (Spec.Apply Spec.Hole Spec.Hole)
--                     |> Expect.equal "(_ _)"
--         ]
-- generate : String -> Spec -> String -> Test
-- generate title spec string =
--     describe title
--         [ test "toString" <|
--             \_ -> Expect.equal string <| Spec.toString spec
--         , test "parser" <|
--             \_ -> Expect.equal (Ok spec) <| Parser.run Spec.parser string
--         ]
