module IR.Expr exposing (..)

import IR.Spec as Spec exposing (Spec)



-- TODO: perhaps decorate every tag with a spec


type Expr
    = Variable String
    | Lambda Bool String Expr
    | Closure Bool String Expr
    | Apply Expr Expr
    | Unborrow String Expr
    | Annotation Spec Expr


toString : Expr -> String
toString expr =
    case expr of
        Variable name ->
            name

        Lambda True name body ->
            toString (Closure True name body)

        Lambda False name body ->
            "(" ++ name ++ " -> " ++ toString body ++ ")"

        Closure False name body ->
            "(" ++ name ++ " => " ++ toString body ++ ")"

        Closure True name body ->
            "(*" ++ name ++ " => " ++ toString body ++ ")"

        Apply function argument ->
            "(" ++ toString function ++ " " ++ toString argument ++ ")"

        Unborrow name body ->
            "unborrow " ++ name ++ " in " ++ toString body

        Annotation spec expr_ ->
            "<" ++ Spec.toString spec ++ ">" ++ toString expr_
