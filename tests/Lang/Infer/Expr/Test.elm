module Lang.Infer.Expr.Test exposing (suite)

import Expect
import Lang.Canonical.Expr exposing (Expr)
import Lang.Canonical.Type as Type
import Lang.Infer.Error exposing (Error(..))
import Lang.Infer.Expr as Infer
import Lang.Infer.Expr.Fixtures as Fixtures
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
            ]
        ]


toResult : Expr -> Result String String
toResult expr =
    case Infer.infer expr State.empty of
        Return thisT _ ->
            Ok (Type.toString thisT)

        Throw (Error msg) ->
            Err msg
