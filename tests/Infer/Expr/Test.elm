module Infer.Expr.Test exposing (suite)

import Expect exposing (Expectation)
import IR.Expr as Expr exposing (Expr)
import IR.Spec as Spec
import Infer.Expr as Expr
import Infer.Model as State
import Test exposing (..)


suite : Test
suite =
    describe "Infer.Expr"
        [ describe "infer"
            [ basicInference
            , uniqueness
            ]
        ]


basicInference : Test
basicInference =
    describe "basic inference"
        [ test "identity" <|
            \_ ->
                -- \a -> a
                lam "a" (var "a")
                    |> expectInfer "a -> a"
        , test "always" <|
            \_ ->
                -- \a b -> a
                lam "a" (lam "b" (var "a"))
                    |> expectInfer "a -> b -> a"
        , test "compose" <|
            \_ ->
                -- \f g a -> g (f a)
                lam "f" (lam "g" (lam "a" (app (var "g") (app (var "f") (var "a")))))
                    |> expectInfer "(a -> b) -> (b -> c) -> a -> c"
        , test "apply" <|
            \_ ->
                -- \f a b -> f a b
                lam "f" (lam "a" (lam "b" (app (app (var "f") (var "a")) (var "b"))))
                    |> expectInfer "(a -> b -> c) -> a -> b -> c"
        , test "church two" <|
            \_ ->
                -- \s z -> s (s z)
                lam "s" (lam "z" (app (var "s") (app (var "s") (var "z"))))
                    |> expectInfer "(a -> a) -> a -> a"
        , test "apply twice" <|
            \_ ->
                -- (\f a -> f a a)
                lam "f" (lam "a" (app (app (var "f") (var "a")) (var "a")))
                    |> expectInfer "(a -> a -> b) -> a -> b"
        , test "apply twice always" <|
            \_ ->
                -- (\f a -> f a a) (\a b -> a)
                app (lam "f" (lam "a" (app (app (var "f") (var "a")) (var "a")))) (lam "a" (lam "b" (var "a")))
                    |> expectInfer "a -> a"
        ]


uniqueness : Test
uniqueness =
    describe "uniqueness"
        [ closureCasting
        , linearChecking
        ]


closureCasting : Test
closureCasting =
    describe "closure casting"
        [ test "not when depends on a varying first argument" <|
            \_ ->
                -- \a b c -> a
                lam "a" (lam "b" (var "a"))
                    |> expectInfer "a -> b -> a"
        , test "not when depends on a higher varying argument in a closure" <|
            \_ ->
                -- \a => \b c -> a
                cls "a" (lam "b" (var "a"))
                    |> expectInfer "a => b -> a"
        , test "when depends on a higher linear argument" <|
            \_ ->
                -- \*a => \b -> a
                lin "a" (lam "b" (var "a"))
                    |> expectInfer "*a => b => *a"
        , test "when deeply depends on a higher linear argument" <|
            \_ ->
                -- \*a => \b c -> a
                lin "a" (lam "b" (lam "c" (var "a")))
                    |> expectInfer "*a => b => c => *a"
        ]


linearChecking : Test
linearChecking =
    describe "linear checks"
        [ test "not when reusing varying argument" <|
            \_ ->
                -- \f -> \a => f a a
                lam "f" (lam "a" (app (app (var "f") (var "a")) (var "a")))
                    |> expectInfer "(a -> a -> b) -> a -> b"
        , test "not when reusing varying argument in a closure" <|
            \_ ->
                -- \f -> \a => f a a
                lam "f" (cls "a" (app (app (var "f") (var "a")) (var "a")))
                    |> expectInfer "(a -> a -> b) -> a => b"
        , test "when reusing linear argument" <|
            \_ ->
                -- \f -> \*a => f a a
                lam "f" (lin "a" (app (app (var "f") (var "a")) (var "a")))
                    |> expectInfer "var 'a' previously used"
        ]



-- support


lam : String -> Expr -> Expr
lam =
    Expr.Lambda Spec.Varying


cls : String -> Expr -> Expr
cls =
    Expr.Lambda Spec.Closure


lin : String -> Expr -> Expr
lin =
    Expr.Lambda Spec.Linear


var : String -> Expr
var =
    Expr.Variable


app : Expr -> Expr -> Expr
app =
    Expr.Apply


expectInfer : String -> Expr -> Expectation
expectInfer msg baseExpr =
    Expect.equal msg <|
        case Expr.infer baseExpr State.empty of
            Expr.Return spec state ->
                Spec.toString <| State.unwrap spec state

            Expr.Throw err ->
                err
