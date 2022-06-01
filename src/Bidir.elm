module Bidir exposing (..)

import Dict exposing (Dict)


type alias Name =
    String


type Typ
    = TVar Int
    | TArr Typ Typ



-- TODO: replace Maybe with Spec (where spec is Pending | Declared | Infered)


type Exp
    = Var Spec Name
    | Lam Spec Name (Exp -> Exp)
    | App Spec Exp Exp


type Spec
    = Pending
    | Provided Typ
    | Assembled Typ
    | Error


type Env
    = Env Int Context


type alias Context =
    Dict Int Typ


var : Name -> Exp
var =
    Var Pending


lam : Name -> (Exp -> Exp) -> Exp
lam =
    Lam Pending


app : Exp -> Exp -> Exp
app =
    App Pending


empty =
    Env 0 Dict.empty


type alias Step =
    ( Exp, Env )


check : Exp -> Env -> Step
check exp env =
    case exp of
        Var Pending _ ->
            Debug.todo "Var"

        Lam Pending name body ->
            checkPendingLam name body env

        App Pending func argm ->
            checkPendingApp func argm env

        Var (Assembled _) _ ->
            ( exp, env )

        Lam (Assembled _) _ _ ->
            ( exp, env )

        App (Assembled _) _ _ ->
            ( exp, env )

        _ ->
            let
                _ =
                    Debug.log "check" exp
            in
            Debug.todo "Fuck"


getFreeTVar : Env -> ( Typ, Env )
getFreeTVar (Env count context) =
    let
        tvar =
            TVar count
    in
    ( tvar, Env (count + 1) (Dict.insert count tvar context) )


lookAhead : Env -> Env -> Maybe Typ
lookAhead (Env key _) (Env _ context) =
    Dict.get key context


checkPendingLam : String -> (Exp -> Exp) -> Env -> Step
checkPendingLam name body env =
    let
        ( tvar, env1 ) =
            getFreeTVar env

        ( body_, env2 ) =
            check (body (Var (Assembled tvar) name)) env1
    in
    case lookAhead env env2 of
        Just argT ->
            case getTyp (getSpec body_)  of
                Just retT ->
                    ( Lam (Assembled (TArr argT retT)) name body
                    , env
                    )

                Nothing ->
                    Debug.todo "AAAAA"

        Nothing ->
            Debug.todo "BBBBB"

checkPendingApp : Exp -> Exp -> Env -> Step
checkPendingApp func argm env =
    case func of
        Var spec name ->
            let
                ( argm_, _ ) =
                    check argm env

                spec_ =
                    case getTyp spec of
                        Just (TVar key) ->
                            case getTyp (getSpec argm_) of
                                Just typ ->
                                    Assembled (TArr (TVar key) typ)

                                Nothing ->
                                    Assembled (TVar key)

                        _ ->
                            Pending
            in
            (Var spec_ name, env)

        Lam _ _ _ ->
            Debug.todo "AppLam"
        App _ _ _ -> 
            Debug.todo "AppApp"


inc : (b -> b) -> b -> b
inc s z =
    s (s z)

getSpec : Exp -> Spec
getSpec exp =
    case exp of
        Var spec _ ->
            spec

        Lam spec _ _ ->
            spec

        App spec _ _ ->
            spec


getTyp : Spec -> Maybe Typ
getTyp spec =
    case spec of
        Assembled t ->
            Just t

        Provided t ->
            Just t

        _ ->
            Nothing


extend : Int -> Typ -> Env -> Env
extend key typ (Env count context) =
    Env count (Dict.insert key typ context)

