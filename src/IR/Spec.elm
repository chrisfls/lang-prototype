module IR.Spec exposing (..)

import Dict exposing (Dict)


type Spec
    = Reference Address
    | Arrow Bool (Maybe String) Spec Spec
    | Linear Spec
    | Unborrow String Spec


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

        Linear (Arrow linear name argument return) ->
            arrowToString True linear name argument return state

        Arrow linear name argument return ->
            arrowToString False linear name argument return state


        Linear subSpec ->
            toStringHelp subSpec state

        Unborrow name subSpec ->
            let
                ( subSpecString, newState ) =
                    toStringHelp subSpec state
            in
            ( "[unborrow " ++ name ++ "] " ++ subSpecString, newState )


arrowToString : Bool -> Bool -> Maybe String -> Spec -> Spec -> ToStringState -> ( String, ToStringState )
arrowToString fatArrow linear maybeName argument return state =
    let
        ( argumentString, newState ) =
            case argument of
                Arrow _ _ _ _ ->
                    Tuple.mapFirst wrapParens <| toStringHelp argument state

                _ ->
                    toStringHelp argument state

        ( returnString, finalState ) =
            toStringHelp return newState

        arrow =
            if fatArrow then
                " => "

            else
                " -> "

        prefix =
            if linear then
                case maybeName of
                    Just name ->
                        "*" ++ name ++ ": "

                    Nothing ->
                        "*"

            else
                case maybeName of
                    Just name ->
                        name ++ ": "

                    Nothing ->
                        ""
    in
    ( prefix ++ argumentString ++ arrow ++ returnString, finalState )


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
