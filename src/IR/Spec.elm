module IR.Spec exposing (..)

import Dict exposing (Dict)



-- TODO: remove maybe from arrow's name


type Spec
    = Reference Bool Address
    | Arrow Linearity (Maybe String) Spec Spec
    | Unborrow String Spec


type Linearity
    = Varying
    | Closure
    | Linear


type alias Address =
    Int


toString : Spec -> String
toString t =
    Tuple.first (toStringHelp t (ToStringState 0 Dict.empty))



--- internal


type alias ToStringState =
    { count : Int, cache : Dict Int String }


toStringHelp : Spec -> ToStringState -> ( String, ToStringState )
toStringHelp spec state =
    case spec of
        Reference _ address ->
            getVarName address state

        Arrow linearity name argument return ->
            arrowToString linearity name argument return state

        Unborrow name subSpec ->
            let
                ( subSpecString, newState ) =
                    toStringHelp subSpec state
            in
            ( "~" ++ name ++ " " ++ subSpecString, newState )


arrowToString : Linearity -> Maybe String -> Spec -> Spec -> ToStringState -> ( String, ToStringState )
arrowToString linearity maybeName argument return state =
    let
        ( argumentString, newState ) =
            case argument of
                Arrow _ _ _ _ ->
                    Tuple.mapFirst wrapParens <| toStringHelp argument state

                _ ->
                    toStringHelp argument state

        argName =
            case linearity of
                Linear ->
                    case maybeName of
                        Just name ->
                            "(*" ++ name ++ ": " ++ argumentString ++ ")"

                        Nothing ->
                            "*" ++ argumentString

                _ ->
                    case maybeName of
                        Just name ->
                            "(" ++ name ++ ": " ++ argumentString ++ ")"

                        Nothing ->
                            argumentString

        arrow =
            case linearity of
                Varying ->
                    " -> "

                _ ->
                    " => "

        ( returnString, finalState ) =
            toStringHelp return newState
    in
    ( argName ++ arrow ++ returnString, finalState )


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
