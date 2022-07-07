module Lang.Canonical.Type exposing (Type(..), toString)

import Dict exposing (Dict)



-- TODO: Add module type


type Type
    = Var Int
    | Arr Type Type
    | Tup (List Type)


toString : Type -> String
toString typeT =
    Tuple.first (toStringHelp typeT (ToStringState 0 Dict.empty))



--- internal


type alias ToStringState =
    { count : Int, cache : Dict Int String }


toStringHelp : Type -> ToStringState -> ( String, ToStringState )
toStringHelp typeT state =
    case typeT of
        Var index ->
            getVarName index state

        Arr ((Arr _ _) as f) t ->
            let
                ( fstr, newState ) =
                    toStringHelp f state

                ( tstr, finalState ) =
                    toStringHelp t newState
            in
            ( "(" ++ fstr ++ ") -> " ++ tstr, finalState )

        Arr f t ->
            let
                ( fstr, newState ) =
                    toStringHelp f state

                ( tstr, finalState ) =
                    toStringHelp t newState
            in
            ( fstr ++ " -> " ++ tstr, finalState )

        Tup ts ->
            let
                ( types, finalState ) =
                    List.foldr
                        (\t (xs, nextState) ->
                            let
                                (str, nextState_) =
                                    toStringHelp t nextState
                            in
                            ( str :: xs, nextState_)
                        )
                        ([], state)
                        ts
            in
            ( "{ " ++ String.join ", " types ++ " }", finalState)


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
