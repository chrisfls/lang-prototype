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
                    |> expectInfer "(a: a) -> a"
        , test "always" <|
            \_ ->
                -- \a b -> a
                lam "a" (lam "b" (var "a"))
                    |> expectInfer "(a: a) -> (b: b) -> a"
        , test "compose" <|
            \_ ->
                -- \f g a -> g (f a)
                lam "f" (lam "g" (lam "a" (app (var "g") (app (var "f") (var "a")))))
                    |> expectInfer "(f: (a -> b)) -> (g: (b -> c)) -> (a: a) -> c"
        , test "apply" <|
            \_ ->
                -- \f a b -> f a b
                lam "f" (lam "a" (lam "b" (app (app (var "f") (var "a")) (var "b"))))
                    |> expectInfer "(f: (a -> b -> c)) -> (a: a) -> (b: b) -> c"
        , test "church two" <|
            \_ ->
                -- \s z -> s (s z)
                lam "s" (lam "z" (app (var "s") (app (var "s") (var "z"))))
                    |> expectInfer "(s: (a -> a)) -> (z: a) -> a"
        , test "apply twice" <|
            \_ ->
                -- (\f a -> f a a)
                lam "f" (lam "a" (app (app (var "f") (var "a")) (var "a")))
                    |> expectInfer "(f: (a -> a -> b)) -> (a: a) -> b"
        , test "apply twice always" <|
            \_ ->
                -- (\f a -> f a a) (\a b -> a)
                app (lam "f" (lam "a" (app (app (var "f") (var "a")) (var "a")))) (lam "a" (lam "b" (var "a")))
                    |> expectInfer "(a: a) -> a"
        ]


uniqueness : Test
uniqueness =
    describe "uniqueness"
        [ closureCasting
        ]



-- TODO: refactor this into checking that var was used
-- borrowChecking : Test
-- borrowChecking =
--     describe "borrow checking"
--         [ test "when unborrowing a varying argument" <|
--             \_ ->
--                 -- \a -> free a in \b -> b
--                 lam "a" (unb "a" (lam "b" (var "b")))
--                     |> expectInfer "(a: a) -> (b: b) -> b"
--         , test "not when using an unborrowed varying argument" <|
--             \_ ->
--                 -- \a -> free a in \b -> a
--                 lam "a" (unb "a" (lam "b" (var "a")))
--                     |> expectInfer "var 'a' already disposed"
--         , test "when unborrowing a varying argument in a closure" <|
--             \_ ->
--                 -- \a => free a in \b -> b
--                 cls "a" (unb "a" (lam "b" (var "b")))
--                     |> expectInfer "(a: a) => (b: b) -> b"
--         , test "not when using an unborrowed varying argument in a closure" <|
--             \_ ->
--                 -- \a => free a in \b -> a
--                 cls "a" (unb "a" (lam "b" (var "a")))
--                     |> expectInfer "var 'a' already disposed"
--         , test "when unborrowing a linear argument" <|
--             \_ ->
--                 -- \*a => free a in \b -> b
--                 lin "a" (unb "a" (lam "b" (var "b")))
--                     |> expectInfer "(*a: a) => (b: b) -> b"
--         , test "not when using an unborrowed linear argument" <|
--             \_ ->
--                 -- \*a => free a in \b -> a
--                 lin "a" (unb "a" (lam "b" (var "a")))
--                     |> expectInfer "var 'a' already disposed"
--         , only <|
--             test "apply twice always" <|
--                 \_ ->
--                     -- (\f a -> f a a) (\*a b => a)
--                     app (lam "f" (lam "a" (app (app (var "f") (var "a")) (var "a")))) (cls "a" (lam "b" (var "a")))
--                         |> expectInfer "(a: a) -> a"
--         -- , only <| test "pamonha" <|
--         --     \_ ->
--         --         -- (\a => b -> a) (\a -> a)
--         --         (app (cls "a" (lam "b" (var "a"))) (lin "a" (var "a")))
--         --             |> expectInfer "(b: a) -> (a: b) -> b"
--         ]


closureCasting : Test
closureCasting =
    describe "closure casting"
        [ test "not when depends on a varying first argument" <|
            \_ ->
                -- \a b c -> a
                lam "a" (lam "b" (var "a"))
                    |> expectInfer "(a: a) -> (b: b) -> a"
        , test "not when depends on a higher varying argument in a closure" <|
            \_ ->
                -- \a => \b c -> a
                cls "a" (lam "b" (var "a"))
                    |> expectInfer "(a: a) => (b: b) -> a"
        , test "when depends on a higher linear argument" <|
            \_ ->
                -- \*a => \b -> a
                lin "a" (lam "b" (var "a"))
                    |> expectInfer "(*a: a) => (b: b) => a"
        , test "when deeply depends on a higher linear argument" <|
            \_ ->
                -- \*a => \b c -> a
                lin "a" (lam "b" (lam "c" (var "a")))
                    |> expectInfer "(*a: a) => (b: b) => (c: c) => a"
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
