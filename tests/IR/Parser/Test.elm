module IR.Parser.Test exposing (suite)

import Expect exposing (Expectation)
import IR.Annotation as Annotation exposing (Annotation)
import IR.Linearity as Linearity
import IR.Parser.Expr as Expr
import IR.Parser.Spec as Spec
import Parser
import Test exposing (..)


suite : Test
suite =
    describe "IR.Parser"
        [ bindings
        , annotations
        ]


bindings : Test
bindings =
    describe "bindings"
        [ test "lowercase start" <|
            \_ ->
                Parser.run Expr.var
                    "a"
                    |> Expect.equal (Ok "a")
        , test "underscore start" <|
            \_ ->
                Parser.run Expr.var
                    "_a"
                    |> Expect.equal (Ok "_a")
        , test "uppercase start" <|
            \_ ->
                Parser.run Expr.var
                    "A"
                    |> Expect.err
        , test "number start" <|
            \_ ->
                Parser.run Expr.var
                    "1"
                    |> Expect.err
        , test "single tick end" <|
            \_ ->
                Parser.run Expr.var
                    "a'"
                    |> Expect.equal (Ok "a'")
        , test "multi tick end" <|
            \_ ->
                Parser.run Expr.var
                    "a''"
                    |> Expect.equal (Ok "a''")
        , test "complex name" <|
            \_ ->
                Parser.run Expr.var
                    "aB_xy_''"
                    |> Expect.equal (Ok "aB_xy_''")
        ]


annotations : Test
annotations =
    describe "annotations"
        [ varyingReference
        , linearReference
        , arrows
        ]


varyingReference : Test
varyingReference =
    describe "varying references"
        [ test "lowercase start" <|
            \_ ->
                Parser.run Spec.annotation "a"
                    |> Expect.equal (Ok (var "a"))
        , test "no underscore start" <|
            \_ ->
                Parser.run Spec.annotation "_a"
                    |> Expect.notEqual (Ok (var "_a"))
        , test "no uppercase start" <|
            \_ ->
                Parser.run Spec.annotation "A"
                    |> Expect.notEqual (Ok (var "A"))
        , test "no number start" <|
            \_ ->
                Parser.run Spec.annotation "1"
                    |> Expect.notEqual (Ok (var "1"))
        , test "single tick end" <|
            \_ ->
                Parser.run Spec.annotation "a'"
                    |> Expect.equal (Ok (var "a'"))
        , test "multi tick end" <|
            \_ ->
                Parser.run Spec.annotation "a''"
                    |> Expect.equal (Ok (var "a''"))
        ]


linearReference : Test
linearReference =
    describe "linear references"
        [ test "lowercase start" <|
            \_ ->
                Parser.run Spec.annotation "*a"
                    |> Expect.equal (Ok (ref "a"))
        , test "no underscore start" <|
            \_ ->
                Parser.run Spec.annotation "*_a"
                    |> Expect.notEqual (Ok (ref "_a"))
        , test "no uppercase start" <|
            \_ ->
                Parser.run Spec.annotation "*A"
                    |> Expect.notEqual (Ok (ref "A"))
        , test "number start" <|
            \_ ->
                Parser.run Spec.annotation "*1"
                    |> Expect.notEqual (Ok (ref "1"))
        , test "single tick end" <|
            \_ ->
                Parser.run Spec.annotation "*a'"
                    |> Expect.equal (Ok (ref "a'"))
        , test "multi tick end" <|
            \_ ->
                Parser.run Spec.annotation "*a''"
                    |> Expect.equal (Ok (ref "a''"))
        , test "space after star" <|
            \_ ->
                Parser.run Spec.annotation "* a"
                    |> Expect.equal (Ok (ref "a"))
        ]


arrows : Test
arrows =
    describe "arrows"
        [ test "single param" <|
            \_ ->
                Parser.run Spec.annotation "a -> b"
                    |> Expect.equal
                        (Ok (lam (var "a") (var "b")))
        , test "two params" <|
            \_ ->
                Parser.run Spec.annotation "a -> b -> c"
                    |> Expect.equal
                        (Ok (lam (var "a") (lam (var "b") (var "c"))))
        , test "three params" <|
            \_ ->
                Parser.run Spec.annotation "a -> b -> c -> d"
                    |> Expect.equal
                        (Ok (lam (var "a") (lam (var "b") (lam (var "c") (var "d")))))
        ]



-- support


lam : Annotation -> Annotation -> Annotation
lam =
    Annotation.Arrow Linearity.Varying


cls : Annotation -> Annotation -> Annotation
cls =
    Annotation.Arrow Linearity.Closure


var : String -> Annotation
var =
    Annotation.Reference False


ref : String -> Annotation
ref =
    Annotation.Reference True