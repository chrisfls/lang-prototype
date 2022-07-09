module Lang.Canonical.Expr.Test exposing (..)

import Expect
import Lang.Canonical.Expr as Expr
import Lang.Canonical.Expr.Fixtures as Fixtures
import Lang.Infer.Error exposing (Error(..))
import Lang.Infer.Return exposing (Return(..))
import Test exposing (..)


suite : Test
suite =
    describe "Lang.Canonical.Expr"
        [ describe "toString"
            [ test "\\a b -> (a , b)" <|
                \_ ->
                    Expr.toString Fixtures.pair
                        |> Expect.equal "(a -> (b -> ( a, b )))"
            ]
        ]
