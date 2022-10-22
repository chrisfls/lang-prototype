module Infer.ModuleBody.Test exposing (suite)

import Expect exposing (Expectation)
import IR.Annotation as Annotation exposing (Annotation)
import IR.Expr as Expr exposing (Expr)
import IR.Linearity as Linearity
import IR.Module as Module exposing (ModuleBody)
import IR.Spec as Spec
import Infer.Model as Model
import Infer.ModuleBody as ModuleBody
import Test exposing (..)


suite : Test
suite =
    describe "Infer.ModuleBody"
        [ test "test" <|
            \_ ->
                defExpr "identity"
                    (lam "a" (var "a"))
                    return
                    |> expectInfer "struct { identity : a -> a }"
        ]



-- support


defSpec : String -> Annotation -> ModuleBody -> ModuleBody
defSpec =
    Module.DefSpec


defpSpec : String -> Annotation -> ModuleBody -> ModuleBody
defpSpec =
    Module.DefSpec


defExpr : String -> Expr -> ModuleBody -> ModuleBody
defExpr =
    Module.DefExpr


defpExpr : String -> Expr -> ModuleBody -> ModuleBody
defpExpr =
    Module.DefExpr


return : ModuleBody
return =
    Module.ReturnModule


lam : String -> Expr -> Expr
lam =
    Expr.Lambda Linearity.Varying


var : String -> Expr
var =
    Expr.Variable


app : Expr -> Expr -> Expr
app =
    Expr.Apply


expectInfer : String -> ModuleBody -> Expectation
expectInfer msg baseModule =
    Expect.equal msg <|
        case ModuleBody.infer baseModule Model.empty of
            ModuleBody.Return spec _ ->
                Spec.toString <| Spec.Struct spec

            ModuleBody.Throw err ->
                err
