module Infer.Expr.Test exposing (suite)

import Expect exposing (Expectation)
import IR.Annotation as Annotation exposing (Annotation)
import IR.Expr as Expr exposing (Expr)
import IR.Linearity as Linearity
import IR.Spec as Spec
import Infer.Expr as Expr
import Infer.Model as Model
import Test exposing (..)



{-

   TODO:

    YOU ONLY LIVE ONCE:

   - [ ] add language parser

   - [\] infer module body

        pending:

            - [\] tests

   - [\] infer module

        pending:

            - [ ] imports
            - [ ] tests

   - [\] add basic codegen

        pending:

            - [ ] tests

   - [ ] add more spec types [PRIORITY]
       - [ ] builtin types
       - [ ] union types
       - [ ] record types
       - [ ] tuple types
   - [ ] add pattern matching
   - [ ] add FFI module
   - [ ] add mutation expressions
   - [ ] add import support
   - [ ] add compiler api

   ...

   - [ ] use inference to optimize codegen
   - [ ] add borrow checker
   - [ ] bootstrap

-}


suite : Test
suite =
    describe "Infer.Expr"
        [ describe "infer"
            [ basicInference
            , typechecking
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


typechecking : Test
typechecking =
    describe "typechecking"
        [ test "when annotation is correct" <|
            \_ ->
                -- assert (a -> a) in \a -> a
                ann (arr (ref "a") (ref "a")) (lam "a" (var "a"))
                    |> expectInfer "a -> a"
        , test "when annotation is wrong" <|
            \_ ->
                -- assert (a -> b) in \a -> a
                ann (arr (ref "a") (ref "b")) (lam "a" (var "a"))
                    |> expectInfer "expected another variable"
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
    Expr.Lambda Linearity.Varying


cls : String -> Expr -> Expr
cls =
    Expr.Lambda Linearity.Closure


lin : String -> Expr -> Expr
lin =
    Expr.Lambda Linearity.Linear


var : String -> Expr
var =
    Expr.Variable


app : Expr -> Expr -> Expr
app =
    Expr.Apply


ann : Annotation -> Expr -> Expr
ann =
    Expr.Annotation


ref : String -> Annotation
ref =
    Annotation.Reference False


arr : Annotation -> Annotation -> Annotation
arr =
    Annotation.Arrow Linearity.Varying


expectInfer : String -> Expr -> Expectation
expectInfer msg baseExpr =
    Expect.equal msg <|
        case Expr.infer baseExpr Model.empty of
            Expr.Return spec state ->
                Spec.toString <| Model.unwrapSpec spec state

            Expr.Throw err ->
                err
