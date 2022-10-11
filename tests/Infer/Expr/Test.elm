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
            [ basics
            , uniqueness
            ]
        ]


basics : Test
basics =
    describe "basics"
        [ test "identity" <|
            \_ ->
                -- \a -> a
                lam "a" (var "a")
                    |> expectInfer "a: a -> a"
        , test "always" <|
            \_ ->
                -- \a b -> a
                lam "a" (lam "b" (var "a"))
                    |> expectInfer "a: a -> b: b -> b! a"
        , test "compose" <|
            \_ ->
                -- \f g a -> g (f a)
                lam "f" (lam "g" (lam "a" (app (var "g") (app (var "f") (var "a")))))
                    |> expectInfer "f: (a -> b) -> g: (b -> c) -> a: a -> c"
        , test "apply" <|
            \_ ->
                -- \f a b -> f a b
                lam "f" (lam "a" (lam "b" (app (app (var "f") (var "a")) (var "b"))))
                    |> expectInfer "f: (a -> b -> c) -> a: a -> b: b -> c"
        , test "church two" <|
            \_ ->
                -- \s z -> s (s z)
                lam "s" (lam "z" (app (var "s") (app (var "s") (var "z"))))
                    |> expectInfer "s: (a -> a) -> z: a -> a"
        ]


uniqueness : Test
uniqueness =
    describe "uniqueness"
        [ casts
        , unborrow
        ]


casts : Test
casts =
    describe "cast"
        [ describe "lambda" <|
            [ test "to linear when depends on higher linear arg" <|
                \_ ->
                    -- \*a b => a
                    laml "a" (lam "b" (var "a"))
                        |> expectInfer "*a: a => b: b => b! a"
            ]
        , describe "closure (linear arg)" <|
            [ test "to linear when depends on higher linear arg" <|
                \_ ->
                    -- \*a b => a
                    clsl "a" (lam "b" (var "a"))
                        |> expectInfer "*a: a => b: b => b! a"
            ]
        ]


unborrow : Test
unborrow =
    describe "unborrow"
        [ automaticUnborrow
        , manualUnborrow
        ]


automaticUnborrow : Test
automaticUnborrow =
    describe "automatic"
        [ describe "lambda" <|
            [ test "first arg" <|
                \_ ->
                    -- \a b -> a
                    lam "a" (lam "b" (var "b"))
                        |> expectInfer "a: a -> a! b: b -> b"
            , test "second arg" <|
                \_ ->
                    -- \a b -> a
                    lam "a" (lam "b" (var "a"))
                        |> expectInfer "a: a -> b: b -> b! a"
            ]
        , describe "closure" <|
            [ test "first arg" <|
                \_ ->
                    -- \a => (\b -> a)
                    cls "a" (lam "b" (var "b"))
                        |> expectInfer "a: a => a! b: b -> b"
            , test "second arg" <|
                \_ ->
                    -- \a -> (\b => a)
                    lam "a" (cls "b" (var "a"))
                        |> expectInfer "a: a -> b: b => b! a"
            ]
        , describe "closure (linear arg)"
            [ test "first arg" <|
                \_ ->
                    -- \*a => (\b -> a)
                    clsl "a" (lam "b" (var "b"))
                        |> expectInfer "*a: a => a! b: b -> b"
            , test "second arg" <|
                \_ ->
                    -- \a -> (\*b => a)
                    lam "a" (clsl "b" (var "a"))
                        |> expectInfer "a: a -> *b: b => b! a"
            ]
        ]


manualUnborrow : Test
manualUnborrow =
    describe "manual"
        [ describe "lambda"
            [ test "first arg" <|
                \_ ->
                    -- \a -> free a in \b -> b
                    lam "a" (unb "a" (lam "b" (var "b")))
                        |> expectInfer "a: a -> a! b: b -> b"
            , test "error after unborrow" <|
                \_ ->
                    -- \a -> free a in \b -> a
                    lam "a" (unb "a" (lam "b" (var "a")))
                        |> expectInfer "var 'a' already disposed"
            ]
        , describe "closure"
            [ test "first arg" <|
                \_ ->
                    -- \a => free a in \b -> b
                    cls "a" (unb "a" (lam "b" (var "b")))
                        |> expectInfer "a: a => a! b: b -> b"
            , test "error after unborrow" <|
                \_ ->
                    -- \a => free a in \b -> a
                    cls "a" (unb "a" (lam "b" (var "a")))
                        |> expectInfer "var 'a' already disposed"
            ]
        , describe "closure (linear arg)"
            [ test "first arg" <|
                \_ ->
                    -- \*a => free a in \b -> b
                    clsl "a" (unb "a" (lam "b" (var "b")))
                        |> expectInfer "*a: a => a! b: b -> b"
            , test "error after unborrow" <|
                \_ ->
                    -- \*a => free a in \b -> a
                    clsl "a" (unb "a" (lam "b" (var "a")))
                        |> expectInfer "var 'a' already disposed"
            ]
        ]



-- support


lam : String -> Expr -> Expr
lam =
    Expr.Lambda False False


laml : String -> Expr -> Expr
laml =
    Expr.Lambda False True


cls : String -> Expr -> Expr
cls =
    Expr.Lambda True False


clsl : String -> Expr -> Expr
clsl =
    Expr.Lambda True True


var : String -> Expr
var =
    Expr.Variable


app : Expr -> Expr -> Expr
app =
    Expr.Apply


unb : String -> Expr -> Expr
unb =
    Expr.Unborrow


expectInfer : String -> Expr -> Expectation
expectInfer msg baseExpr =
    Expect.equal msg <|
        case Expr.infer baseExpr State.empty of
            Expr.Return spec state ->
                Spec.toString <| State.unwrap spec state

            Expr.Throw err ->
                err
