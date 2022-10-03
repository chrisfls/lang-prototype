module IR.Spec exposing (..)

import Dict exposing (Dict)


type Spec
    = Reference Address
    | Arrow (Maybe Linear) (Maybe String) Spec Spec
    | Linear Spec
    | Free String Spec


type alias Address =
    Int


type alias Linear =
    Bool


toString : Spec -> String
toString t =
    Tuple.first (toStringHelp t (ToStringState 0 Dict.empty))



--- internal


type alias ToStringState =
    { count : Int, cache : Dict Int String }


toStringHelp : Spec -> ToStringState -> ( String, ToStringState )
toStringHelp spec state =
    case spec of
        Reference address ->
            getVarName address state

        Arrow linear name argument return ->
            arrowToString linear name argument return state

        Linear ((Arrow _ _ _ _) as arrow) ->
            toStringHelp arrow state

        Linear subSpec ->
            toStringHelp subSpec state

        Free name subSpec ->
            let
                ( subSpecString, newState ) =
                    toStringHelp subSpec state
            in
            ( "free " ++ name ++ " in " ++ subSpecString, newState )


arrowToString : Maybe Bool -> Maybe String -> Spec -> Spec -> ToStringState -> ( String, ToStringState )
arrowToString linear maybeName argument return state =
    let
        ( argumentString, newState ) =
            case argument of
                Arrow _ _ _ _ ->
                    Tuple.mapFirst wrapParens <| toStringHelp argument state

                _ ->
                    toStringHelp argument state

        ( returnString, finalState ) =
            toStringHelp return newState
    in
    case maybeName of
        Just name ->
            case linear of
                Just True ->
                    ( "*" ++ name ++ ": " ++ argumentString ++ " -> " ++ returnString, finalState )
                _ ->
                    ( name ++ ": " ++ argumentString ++ " -> " ++ returnString, finalState )

        Nothing ->
            case linear of
                Just True ->
                    ( "*" ++ argumentString ++ " -> " ++ returnString, finalState )

                _ ->
                    ( argumentString ++ " -> " ++ returnString, finalState )


wrapParens : String -> String
wrapParens str =
    "(" ++ str ++ ")"


getVarName : Address -> ToStringState -> ( String, ToStringState )
getVarName address state =
    case Dict.get address state.cache of
        Just name ->
            ( name, state )

        Nothing ->
            let
                { count, cache } =
                    state

                name =
                    getVarNameHelp count
            in
            ( name, { count = count + 1, cache = Dict.insert address name cache } )


getVarNameHelp : Address -> String
getVarNameHelp address =
    case address of
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
            getVarNameHelp (modBy 4 address) ++ String.fromInt (address + 1)
