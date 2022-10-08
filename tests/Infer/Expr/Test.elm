module Infer.Expr.Test exposing (suite)

import Expect
import IR.Expr as Expr exposing (Expr)
import IR.Spec as Spec
import Infer.Expr as Expr
import Infer.State as State
import Test exposing (..)


lam =
    Expr.Lambda Nothing


cls =
    Expr.Lambda (Just True)


var =
    Expr.Variable


app =
    Expr.Apply


suite : Test
suite =
    describe "Infer.Expr"
        [ describe "infer"
            [ test "\\s z -> s (s z)" <|
                \_ ->
                    lam "s" (lam "z" (app (var "s") (app (var "s") (var "z"))))
                        |> toResult
                        |> Expect.equal (Ok "s: (a -> a) -> z: a -> a")
            , test "\\f g a -> g (f a)" <|
                \_ ->
                    lam "f" (lam "g" (lam "a" (app (var "g") (app (var "f") (var "a")))))
                        |> toResult
                        |> Expect.equal (Ok "f: (a -> b) -> g: (b -> c) -> a: a -> c")
            , test "\\f a b -> f a b" <|
                \_ ->
                    lam "f" (lam "a" (lam "b" (app (app (var "f") (var "a")) (var "b"))))
                        |> toResult
                        |> Expect.equal (Ok "f: (a -> b -> c) -> a: a -> b: b -> c")
            -- , test "\\a *b -> free b in a" <|
            --     \_ ->
            --         lam "a" (cls "b" (var "a"))
            --             |> toResult
            --             |> Expect.equal (Ok "a: a -> *b: b -> free b => a")
            , test "\\a b -> a" <|
                \_ ->
                    lam "a" (lam "b" (var "a"))
                        |> toResult
                        |> Expect.equal (Ok "a: a -> b: b -> unborrow b => a")
            , only <| test "\\a b -> b" <|
                -- TODO: find a way to unborrow a early
                \_ ->
                    lam "a" (lam "b" (var "b"))
                        |> toResult
                        |> Expect.equal (Ok "a: a -> unborrow a => b: b -> b")

            -- , test "\\*a b -> b" <|
            --         \_ ->
            --             cls "a" (lam "b" (var "b"))
            --                 |> toResult
            --                 |> Expect.equal (Ok "*a: a -> free a => b: b -> b")
            ]
        ]


toResult : Expr -> Result String String
toResult expr =
    case Expr.infer expr State.empty of
        Ok { spec, state } ->
            Ok (Spec.toString <| State.unwrap spec state)

        Err msg ->
            Err msg
