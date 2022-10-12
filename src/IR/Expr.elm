module IR.Expr exposing (..)

import IR.Spec as Spec exposing (Linearity, Spec)



-- TODO: perhaps decorate every tag with a spec


type Expr
    = Variable String
    | Lambda Linearity String Expr
    | Apply Expr Expr
    | Unborrow String Expr
    | Annotation Spec Expr


toString : Expr -> String
toString expr =
    case expr of
        Variable name ->
            name

        Lambda Spec.Linear name body ->
            "(*" ++ name ++ " => " ++ toString body ++ ")"

        Lambda Spec.Closure name body ->
            "(" ++ name ++ " => " ++ toString body ++ ")"

        Lambda Spec.Varying name body ->
            "(" ++ name ++ " -> " ++ toString body ++ ")"

        Apply function argument ->
            "(" ++ toString function ++ " " ++ toString argument ++ ")"

        Unborrow name body ->
            "unborrow " ++ name ++ " in " ++ toString body

        Annotation spec expr_ ->
            "<" ++ Spec.toString spec ++ ">" ++ toString expr_
