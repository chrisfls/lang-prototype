module Bidir2 exposing (..)

import Dict exposing (Dict)


type alias Name =
    String


type Typ
    = TVar Int
    | TArr Typ Typ


type Exp
    = Var Name
    | Lam (Maybe Typ) Name (Exp -> Exp)
    | App Exp Exp
    | Ann Typ Exp


type State
    = State Int Env


type alias Env =
    Dict String Typ


type alias Error =
    String


var : Name -> Exp
var =
    Var


lam : Name -> (Exp -> Exp) -> Exp
lam =
    Lam Nothing


app : Exp -> Exp -> Exp
app =
    App


empty : State
empty =
    State 0 Dict.empty


get : String -> State -> Maybe Typ
get name (State _ env) =
    Dict.get name env


insert : String -> Typ -> State -> State
insert name typ (State idx env) =
    State idx (Dict.insert name typ env)


defineFreeTVar : String -> State -> State
defineFreeTVar name (State idx env) =
    State (idx + 1) (Dict.insert name (TVar idx) env)


check : Exp -> State -> Result Error ( Exp, Typ, State )
check exp state =
    case exp of
        Var name ->
            case get name state of
                Just varT ->
                    ( Ann varT exp
                    , varT
                    , state
                    )
                        |> Ok

                Nothing ->
                    Err ("Var " ++ name ++ " not found")

        Lam Nothing name body ->
            inferLam name body state

        App func argm ->
            case check argm state of
                Ok ( argm_, argmT, state_ ) ->
                    inferApp func argm_ argmT state

                Err e ->
                    Err e

        _ ->
            Err ("Can't infer" ++ toString exp)


inferLam : String -> (Exp -> Exp) -> State -> Result Error ( Exp, Typ, State )
inferLam name body state =
    case check (body (Var name)) (defineFreeTVar name state) of
        Ok ( _, bodyT, state_ ) ->
            case get name state_ of
                Just argmT ->
                    let
                        typ =
                            TArr argmT bodyT
                    in
                    ( Lam (Just typ) name body
                    , typ
                    , state_
                    )
                        |> Ok

                Nothing ->
                    Err ("Var " ++ name ++ " not found2")

        Err e ->
            Err e


inferApp : Exp -> Exp -> Typ -> State -> Result Error ( Exp, Typ, State )
inferApp func argm argmT state =
    case func of
        Var name ->
            case get name state of
                Just typ ->
                    case applyType typ argmT of
                        Ok ( bodyT, funcT ) ->
                            ( Ann bodyT (App func argm)
                            , bodyT
                            , insert name funcT state
                            )
                                |> Ok

                        Err e ->
                            Err e

                Nothing ->
                    Err ("Var " ++ name ++ " not found3")

        _ ->
            Err ("can't infer app " ++ toString func ++ " " ++ toString argm)


applyType : Typ -> Typ -> Result Error ( Typ, Typ )
applyType funcT argmT =
    case funcT of
        TVar _ ->
            ( argmT
            , TArr argmT funcT
            )
                |> Ok

        TArr (TVar _) bodyT ->
            ( bodyT
            , TArr argmT bodyT
            )
                |> Ok

        TArr l r ->
            Err ("can't applyType " ++ toStringTyp l ++ toStringTyp r)


--- this is not important:


toString : Exp -> String
toString exp =
    case exp of
        Var name ->
            name

        Lam (Just typ) name body ->
            "(" ++ name ++ "[" ++ toStringTyp typ ++ "]" ++ "." ++ toString (body (Var name))

        Lam Nothing name body ->
            "(" ++ name ++ "." ++ toString (body (Var name))

        App func argm ->
            "(" ++ toString func ++ " " ++ toString argm ++ ")"

        Ann typ exp_ ->
            toString exp_ ++ "[" ++ toStringTyp typ ++ "]"


toStringTyp : Typ -> String
toStringTyp typ =
    case typ of
        TVar i ->
            toStringTVar i

        TArr ((TArr _ _) as f) ((TArr _ _) as t) ->
            "(" ++ toStringTyp f ++ ").(" ++ toStringTyp t ++ ")"

        TArr ((TArr _ _) as f) ((TVar _) as t) ->
            "(" ++ toStringTyp f ++ ")." ++ toStringTyp t

        TArr ((TVar _) as f) ((TArr _ _) as t) ->
            toStringTyp f ++ ".(" ++ toStringTyp t ++ ")"

        TArr ((TVar _) as f) ((TVar _) as t) ->
            toStringTyp f ++ "." ++ toStringTyp t


toStringTVar : Int -> String
toStringTVar arg =
    let
        arg_ =
            max 0 (arg - 12)
    in
    case List.head (List.drop (modBy 12 arg) vars) of
        Just a ->
            if arg_ > 0 then
                a ++ String.fromInt arg_

            else
                a

        Nothing ->
            "unk" ++ String.fromInt arg


vars : List String
vars =
    String.split "" "abcdefghijkl"


length : Int
length =
    List.length vars