module Infer.Expr.Test exposing (suite)

import Expect
import IR.Expr as Expr exposing (Expr)
import IR.Spec as Spec
import Infer.Expr as Expr
import Infer.Model as State
import Test exposing (..)


lam =
    Expr.Lambda False False


laml =
    Expr.Lambda False True


cls =
    Expr.Lambda True False


clsl =
    Expr.Lambda True True


var =
    Expr.Variable


app =
    Expr.Apply


unb =
    Expr.Unborrow


suite : Test
suite =
    describe "Infer.Expr"
        [ describe "infer"
            -- TODO: better test descriptions
            [ test "\\s z -> s (s z)" <|
                \_ ->
                    lam "s" (lam "z" (app (var "s") (app (var "s") (var "z"))))
                        |> infer
                        |> Expect.equal "s: (a -> a) -> z: a -> a"
            , test "\\f g a -> g (f a)" <|
                \_ ->
                    lam "f" (lam "g" (lam "a" (app (var "g") (app (var "f") (var "a")))))
                        |> infer
                        |> Expect.equal "f: (a -> b) -> g: (b -> c) -> a: a -> c"
            , test "\\f a b -> f a b" <|
                \_ ->
                    lam "f" (lam "a" (lam "b" (app (app (var "f") (var "a")) (var "b"))))
                        |> infer
                        |> Expect.equal "f: (a -> b -> c) -> a: a -> b: b -> c"
            , test "\\a *b -> a" <|
                \_ ->
                    lam "a" (clsl "b" (var "a"))
                        |> infer
                        |> Expect.equal "a: a -> *b: b => b! a"
            , test "\\a b -> a" <|
                \_ ->
                    lam "a" (lam "b" (var "a"))
                        |> infer
                        |> Expect.equal "a: a -> b: b -> b! a"
            , test "\\*a b -> a" <|
                \_ ->
                    clsl "a" (lam "b" (var "a"))
                        |> infer
                        |> Expect.equal "*a: a => b: b => b! a"
            , test "\\a b -> b" <|
                \_ ->
                    lam "a" (lam "b" (var "b"))
                        |> infer
                        |> Expect.equal "a: a -> a! b: b -> b"
            , test "\\a *b -> b" <|
                \_ ->
                    lam "a" (clsl "b" (var "b"))
                        |> infer
                        |> Expect.equal "a: a -> a! *b: b => b"
            , test "\\*a b -> b" <|
                \_ ->
                    clsl "a" (lam "b" (var "b"))
                        |> infer
                        |> Expect.equal "*a: a => a! b: b -> b"
            , manualUnborrow
            ]
        ]

manualUnborrow =
    describe "manual unborrow"
        [ test "minimal valid" <|
            \_ ->
                clsl "a" (unb "a" (lam "b" (var "b")))
                    |> infer
                    |> Expect.equal "*a: a => a! b: b -> b"
        , test "minimal invalid" <|
            \_ ->
                clsl "a" (unb "a" (lam "b" (var "a")))
                    |> infer
                    |> Expect.equal "var 'a' already disposed"
        ]

infer : Expr -> String
infer baseExpr =
    case Expr.infer baseExpr State.empty of
        Expr.Return spec state ->
            Spec.toString <| State.unwrap spec state

        Expr.Throw msg ->
            msg
