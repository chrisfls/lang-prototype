module IR.Expr exposing (..)

import IR.Spec as Spec exposing (Linearity)



-- TODO: perhaps decorate every tag with a spec


type Expr
    = Variable String
    | Lambda Linearity String Expr
    | Apply Expr Expr
    | Annotation Annotation Expr


type Annotation
    = Reference Bool String
    | Arrow Spec.Linearity Annotation Annotation


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

        Annotation spec expr_ ->
            "<" ++ Debug.toString spec ++ ">" ++ toString expr_
