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
                        |> Expect.equal (Ok "a: a -> *b: b -> free b in a")
            , test "\\a b -> free? b in a" <|
                \_ ->
                    toResult Fixtures.discardOrAlways
                        |> Expect.equal (Ok "a: a -> b: b -> free b in a")
            , test "\\a b -> a" <|
                \_ ->
                    toResult Fixtures.always
                        |> Expect.equal (Ok "a: a -> b: b -> a")
            ]
        ]


toResult : Expr -> Result String String
toResult expr =
    case InferExpr.infer expr State.empty of
        Return spec state ->
            Ok (Spec.toString <| State.unwrap spec state)

        Throw msg ->
            Err msg
