module IR.Expr exposing (..)

import IR.Annotation exposing (Annotation)
import IR.Linearity as Linearity exposing (Linearity)



-- TODO: start sourcemaps
-- TODO: InfixApply


type Expr
    = Variable String
    | Lambda Linearity String Expr
    | Apply Expr Expr
    | Annotation Annotation Expr


toString : Expr -> String
toString expr =
    case expr of
        Variable name ->
            name

        Lambda Linearity.Linear name body ->
            "(*" ++ name ++ " => " ++ toString body ++ ")"

        Lambda Linearity.Closure name body ->
            "(" ++ name ++ " => " ++ toString body ++ ")"

        Lambda Linearity.Varying name body ->
            "(" ++ name ++ " -> " ++ toString body ++ ")"

        Apply function argument ->
            "(" ++ toString function ++ " " ++ toString argument ++ ")"

        Annotation spec expr_ ->
            "<" ++ Debug.toString spec ++ ">" ++ toString expr_
