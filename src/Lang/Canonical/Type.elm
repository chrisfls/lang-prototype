module Lang.Canonical.Type exposing (Type(..), toString)

import Dict exposing (Dict)



-- TODO: Add module type


type Type
    = TVar Int
    | TArr Type Type


toString : Type -> String
toString typeT =
    Tuple.first (toStringHelp typeT (ToStringState 0 Dict.empty))



--- internal


type alias ToStringState =
    { count : Int, cache : Dict Int String }


toStringHelp : Type -> ToStringState -> ( String, ToStringState )
toStringHelp typeT state =
    case typeT of
        TVar index ->
            getVarName index state

        TArr ((TArr _ _) as f) t ->
            let
                ( fstr, newState ) =
                    toStringHelp f state

                ( tstr, finalState ) =
                    toStringHelp t newState
            in
            ( "(" ++ fstr ++ ") -> " ++ tstr, finalState )

        TArr f t ->
            let
                ( fstr, newState ) =
                    toStringHelp f state

                ( tstr, finalState ) =
                    toStringHelp t newState
            in
            ( fstr ++ " -> " ++ tstr, finalState )


getVarName : Int -> ToStringState -> ( String, ToStringState )
getVarName index state =
    case Dict.get index state.cache of
        Just name ->
            ( name, state )

        Nothing ->
            let
                { count, cache } =
                    state

                name =
                    getVarNameHelp count
            in
            ( name, { count = count + 1, cache = Dict.insert index name cache } )


getVarNameHelp : Int -> String
getVarNameHelp index =
    case index of
        0 ->
            "a"

        1 ->
            "b"

        2 ->
            "c"

        3 ->
            "d"

        4 ->
            "e"

        _ ->
            getVarNameHelp (modBy 4 index) ++ String.fromInt (index + 1)
