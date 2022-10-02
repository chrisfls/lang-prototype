module Infer.InferExpr.Test exposing (suite)

import Expect
import IR.Expr exposing (Expr)
import IR.Expr.Fixtures as Fixtures
import IR.Spec as Spec
import Infer.InferExpr as InferExpr
import Infer.Return exposing (Return(..))
import Infer.State as State
import Test exposing (..)


suite : Test
suite =
    describe "Lang.Infer.Expr"
        [ describe "infer"
            [ test "\\s z -> s (s z)" <|
                \_ ->
                    toResult Fixtures.two
                        |> Expect.equal (Ok "(a -> a) -> a -> a")
            , test "\\f g a -> g (f a)" <|
                \_ ->
                    toResult Fixtures.compose
                        |> Expect.equal (Ok "(a -> b) -> (b -> c) -> a -> c")
            , test "\\f a b -> f a b" <|
                \_ ->
                    toResult Fixtures.apply2
                        |> Expect.equal (Ok "(a -> b -> c) -> a -> b -> c")
            ]
        ]


toResult : Expr -> Result String String
toResult expr =
    case InferExpr.infer expr State.empty of
        Return spec state ->
            Ok (Spec.toString <| State.unwrap spec state)

        Throw msg ->
            Err msg
