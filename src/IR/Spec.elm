module IR.Spec exposing (..)

import Dict exposing (Dict)
import IR.Linearity as Linearity exposing (Linearity)



-- TODO: start sourcemaps
-- TODO: add struct {} for module format assert
-- TODO: add module() for exposing assert


type Spec
    = Reference Bool Int
    | Arrow Linearity Spec Spec
    | Module Members
    | SpecAt String


type alias Members =
    { exprs : Bindings
    , specs : Bindings
    }


type alias Bindings =
    Dict String Spec


toString : Spec -> String
toString t =
    Tuple.first (toStringHelp t (ToStringState 0 Dict.empty))



--- internal


type alias ToStringState =
    { count : Int, cache : Dict Int String }


toStringHelp : Spec -> ToStringState -> ( String, ToStringState )
toStringHelp spec state =
    case spec of
        Reference linear address ->
            getVarName linear address state

        Arrow linearity argument return ->
            arrowToString linearity argument return state

        SpecAt _ ->
            Debug.todo "SpecAt"

        Module members ->
            membersToString members state


arrowToString : Linearity -> Spec -> Spec -> ToStringState -> ( String, ToStringState )
arrowToString linearity argument return state =
    let
        ( argumentString, newState ) =
            case argument of
                Arrow _ _ _ ->
                    Tuple.mapFirst wrapParens <| toStringHelp argument state

                _ ->
                    toStringHelp argument state

        arrow =
            case linearity of
                Linearity.Varying ->
                    " -> "

                _ ->
                    " => "

        ( returnString, finalState ) =
            toStringHelp return newState
    in
    ( argumentString ++ arrow ++ returnString, finalState )


membersToString : Members -> ToStringState -> ( String, ToStringState )
membersToString members state =
    let
        ( exprs, exprState ) =
            bindingsToString " : " (Dict.toList members.exprs) state

        ( specs, lastState ) =
            bindingsToString " : type" (Dict.toList members.specs) exprState

        bindings =
            String.join ", " <| List.sort (exprs ++ specs)
    in
    ( "module { " ++ bindings ++ " }", lastState )


bindingsToString : String -> List ( String, Spec ) -> ToStringState -> ( List String, ToStringState )
bindingsToString separator bindings state =
    List.foldl
        (\( name, binding ) ( xs, thisState ) ->
            let
                ( str, nextState ) =
                    toStringHelp binding thisState
            in
            ( (name ++ separator ++ str) :: xs, nextState )
        )
        ( [], state )
        bindings


wrapParens : String -> String
wrapParens str =
    "(" ++ str ++ ")"


getVarName : Bool -> Int -> ToStringState -> ( String, ToStringState )
getVarName linear address state =
    case Dict.get address state.cache of
        Just name ->
            ( name, state )

        Nothing ->
            let
                { count, cache } =
                    state

                name =
                    if linear then
                        "*" ++ getVarNameHelp count

                    else
                        getVarNameHelp count
            in
            ( name, { count = count + 1, cache = Dict.insert address name cache } )


getVarNameHelp : Int -> String
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
