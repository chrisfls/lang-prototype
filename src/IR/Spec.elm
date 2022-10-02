module IR.Spec exposing (..)

import Dict exposing (Dict)


type Spec
    = Reference Address
    | Arrow Spec Spec
    | Linear Spec
    | Free Address Spec


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

        Arrow ((Arrow _ _) as argument) return ->
            let
                ( argumentString, newState ) =
                    toStringHelp argument state

                ( returnString, finalState ) =
                    toStringHelp return newState
            in
            ( "(" ++ argumentString ++ ") -> " ++ returnString, finalState )

        Arrow argument return ->
            let
                ( argumentString, newState ) =
                    toStringHelp argument state

                ( returnString, finalState ) =
                    toStringHelp return newState
            in
            ( argumentString ++ " -> " ++ returnString, finalState )

        Linear ((Arrow _ _) as arrow) ->
            let
                ( arrowString, newState ) =
                    toStringHelp arrow state
            in
            ( "(" ++ arrowString ++ ")!", newState )

        Linear subSpec ->
            let
                ( subSpecString, newState ) =
                    toStringHelp subSpec state
            in
            ( subSpecString ++ "!", newState )

        Free _ _ ->
            Debug.todo "TODO FREE "


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
