module Lang.Canonical.Expr exposing (BuiltinValue(..), Expr(..), toString)

import Dict exposing (Dict)
import Lang.Canonical.Type as Type exposing (Type)


-- TODO: pattern match

type Expr
    = Var String
    | Tup (List Expr)
    | Fil Expr String
    | Rec (Dict String Expr)
    | Upd Expr (Dict String Expr)
      -- I might have to replace HOAS with FOAS eventually
    | Lam String (Expr -> Expr)
    | App Expr Expr
    | Ann Type Expr
    | Bul BuiltinValue


type BuiltinValue
    = UnitVal
    | TrueVal
    | FalseVal
    | IntVal Int
    | FloatVal Float
    | StringVal String


toString : Expr -> String
toString exp =
    case exp of
        Var name ->
            name

        Tup list ->
            "( " ++ String.join ", " (List.map toString list) ++ " )"

        Rec fields ->
            "{ "
                ++ (Dict.toList fields
                        |> List.map (\( name, exp_ ) -> name ++ " : " ++ toString exp_)
                        |> String.join ", "
                   )
                ++ " }"

        Upd name fields ->
            "{ "
                ++ toString name
                ++ " | "
                ++ (Dict.toList fields
                        |> List.map (\( name_, exp_ ) -> name_ ++ " : " ++ toString exp_)
                        |> String.join ", "
                   )
                ++ " }"

        Fil body field ->
            toString body ++ "." ++ field

        Lam name body ->
            "(" ++ name ++ " -> " ++ toString (body (Var name)) ++ ")"

        App func argm ->
            "(" ++ toString func ++ " " ++ toString argm ++ ")"

        Ann t (Lam name body) ->
            "(" ++ name ++ " [" ++ Type.toString t ++ "] -> " ++ toString (body (Var name)) ++ ")"

        Ann t exp_ ->
            "<" ++ Type.toString t ++ ">" ++ toString exp_

        Bul raw ->
            case raw of
                UnitVal ->
                    "()"

                TrueVal ->
                    "True"

                FalseVal ->
                    "False"

                IntVal int ->
                    String.fromInt int

                FloatVal float ->
                    String.fromFloat float

                StringVal string ->
                    "\"" ++ String.replace "\"" "\\\"" string ++ "\""
