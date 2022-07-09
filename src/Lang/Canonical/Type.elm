module Lang.Canonical.Type exposing (Type(..), toString)

import Dict exposing (Dict)



-- TODO: add internal types
-- TODO: add union types
-- TODO: add module types (they will work exactly like records)
-- TODO: add linear types

type Type
    = Var Int
    | Arr Type Type
    | Tup (List Type)
    | Rec (Maybe Type) (Dict String Type)


toString : Type -> String
toString t =
    Tuple.first (toStringHelp t (ToStringState 0 Dict.empty))



--- internal


type alias ToStringState =
    { count : Int, cache : Dict Int String }


toStringHelp : Type -> ToStringState -> ( String, ToStringState )
toStringHelp t state =
    case t of
        Var index ->
            getVarName index state

        Arr ((Arr _ _) as arg) ret ->
            let
                ( fstr, newState ) =
                    toStringHelp arg state

                ( tstr, finalState ) =
                    toStringHelp ret newState
            in
            ( "(" ++ fstr ++ ") -> " ++ tstr, finalState )

        Arr arg ret ->
            let
                ( fstr, newState ) =
                    toStringHelp arg state

                ( tstr, finalState ) =
                    toStringHelp ret newState
            in
            ( fstr ++ " -> " ++ tstr, finalState )

        Tup types ->
            let
                ( names, finalState ) =
                    List.foldr
                        (\t_ ( xs, nextState ) ->
                            let
                                ( str, nextState_ ) =
                                    toStringHelp t_ nextState
                            in
                            ( str :: xs, nextState_ )
                        )
                        ( [], state )
                        types
            in
            ( "( " ++ String.join ", " names ++ " )", finalState )

        Rec maybeExt fields ->
            -- TODO: clean this up a bit
            let
                ( names, finalState ) =
                    Dict.foldr
                        (\name t_ ( xs, nextState ) ->
                            let
                                ( str, nextState_ ) =
                                    toStringHelp t_ nextState
                            in
                            ( (name ++ " : " ++ str) :: xs, nextState_ )
                        )
                        ( [], state )
                        fields

                ( ext, finalState_ ) =
                    case maybeExt of
                        Just t_ ->
                            let
                                ( str, fff ) =
                                    toStringHelp t_ finalState
                            in
                            ( str ++ " | ", fff )

                        Nothing ->
                            ( "", finalState )
            in
            ( "{ " ++ ext ++ String.join ", " names ++ " }", finalState_ )


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
