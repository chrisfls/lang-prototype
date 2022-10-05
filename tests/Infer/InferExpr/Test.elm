module Infer.InferExpr.Test exposing (suite)

import Expect
import IR.Expr exposing (Expr)
import IR.Expr.Fixtures as Fixtures
import IR.Spec as Spec
import Infer.InferExpr as InferExpr
import Infer.State as State
import Test exposing (..)


suite : Test
suite =
        describe "Lang.Infer.Expr"
            [ describe "infer"
                [ Test.only <|test "\\s z -> s (s z)" <|
                    \_ ->
                        toResult Fixtures.two
                            |> Expect.equal (Ok "s: (a -> a) -> z: a -> a")
                , test "\\f g a -> g (f a)" <|
                    \_ ->
                        toResult Fixtures.compose
                            |> Expect.equal (Ok "f: (a -> b) -> g: (b -> c) -> a: a -> c")
                , test "\\f a b -> f a b" <|
                    \_ ->
                        toResult Fixtures.apply2
                            |> Expect.equal (Ok "f: (a -> b -> c) -> a: a -> b: b -> c")
                , test "\\a *b -> free b in a" <|
                    \_ ->
                        toResult Fixtures.discard
                            |> Expect.equal (Ok "a: a -> *b: b -> free b => a")
                , test "\\a b -> free b in a" <|
                    \_ ->
                        toResult Fixtures.always
                            |> Expect.equal (Ok "a: a -> b: b -> free b => a")
                , test "WIP" <|
                        \_ ->
                            toResult Fixtures.always2
                                |> Expect.equal (Ok "*a: a -> free a => b: b -> b")
                ]
            ]


toResult : Expr -> Result String String
toResult expr =
    case InferExpr.infer expr State.empty of
        InferExpr.Return _ spec state ->
            Ok (Spec.toString <| State.unwrap spec state)

        InferExpr.Throw msg ->
            Err msg
