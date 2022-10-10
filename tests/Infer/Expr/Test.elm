module Infer.Expr.Test exposing (suite)

import Expect
import IR.Expr as Expr exposing (Expr)
import IR.Spec as Spec
import Infer.Expr as Expr
import Infer.Model as State
import IR.SpecExpr as SpecExpr
import Test exposing (..)


lam =
    Expr.Lambda False


cls =
    Expr.Lambda True


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
            , test "\\a *b -> a" <|
                \_ ->
                    lam "a" (cls "b" (var "a"))
                        |> toResult
                        |> Expect.equal (Ok "a: a -> *b: b -> unborrow b => a")
            , test "\\a b -> a" <|
                \_ ->
                    lam "a" (lam "b" (var "a"))
                        |> toResult
                        |> Expect.equal (Ok "a: a -> b: b -> unborrow b => a")
            , only <| test "\\*a b -> a" <| -- HERE
                \_ ->
                    cls "a" (lam "b" (var "a"))
                        |> toResult
                        |> Expect.equal (Ok "*a: a => b: b => [unborrow b] a")
            , test "\\a b -> b" <|
                \_ ->
                    lam "a" (lam "b" (var "b"))
                        |> toResult
                        |> Expect.equal (Ok "a: a -> unborrow a => b: b -> b")
            , test "\\a *b -> b" <|
                \_ ->
                    lam "a" (cls "b" (var "b"))
                        |> toResult
                        |> Expect.equal (Ok "a: a -> unborrow a => *b: b -> b")
            , test "\\*a b -> b" <|
                    \_ ->
                        cls "a" (lam "b" (var "b"))
                            |> toResult
                            |> Expect.equal (Ok "*a: a -> unborrow a => b: b -> b")
            ]
        ]


toResult : Expr -> Result String String
toResult baseExpr =
    case Expr.toSpecExpr baseExpr State.empty of
        Expr.Return expr state ->
            Ok <| Spec.toString <| State.unwrap (SpecExpr.toSpec expr) state

        Expr.Throw msg ->
            Err msg
