module Lang.Canonical.Expr exposing (Expr(..), toString)

import Lang.Canonical.Type as Type exposing (Type)


type Expr
    = Var String
    | Tup (List Expr)
      -- I might have to replace HOAS with FOAS eventually
    | Lam String (Expr -> Expr)
    | App Expr Expr
    | Ann Type Expr


toString : Expr -> String
toString exp =
    case exp of
        Var n ->
            n

        Tup t ->
            "(" ++ String.join ", " (List.map toString t) ++ ")"

        Lam n b ->
            "(" ++ n ++ " -> " ++ toString (b (Var n)) ++ ")"

        App f a ->
            "(" ++ toString f ++ " " ++ toString a ++ ")"

        Ann t (Lam n b) ->
            "(" ++ n ++ " [" ++ Type.toString t ++ "] -> " ++ toString (b (Var n)) ++ ")"

        Ann t exp_ ->
            "<" ++ Type.toString t ++ ">" ++ toString exp_
