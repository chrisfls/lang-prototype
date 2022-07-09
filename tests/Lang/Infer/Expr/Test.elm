module Lang.Infer.Expr.Test exposing (suite)

import Expect
import Lang.Canonical.Expr exposing (Expr)
import Lang.Canonical.Expr.Fixtures as Fixtures
import Lang.Canonical.Type as Type
import Lang.Infer.Error exposing (Error(..))
import Lang.Infer.Expr as Infer
import Lang.Infer.Return exposing (Return(..))
import Lang.Infer.State as State
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
            , test "\\a b -> ( a, b )" <|
                \_ ->
                    toResult Fixtures.pair
                        |> Expect.equal (Ok "a -> b -> ( a, b )")
            , test "\\a b -> { a = a, b = b }" <|
                \_ ->
                    toResult Fixtures.record
                        |> Expect.equal (Ok "a -> b -> { a : a, b : b }")
            ]
        ]


toResult : Expr -> Result String String
toResult expr =
    case Infer.infer expr State.empty of
        Return t _ ->
            Ok (Type.toString t)

        Throw (Error msg) ->
            Err msg
