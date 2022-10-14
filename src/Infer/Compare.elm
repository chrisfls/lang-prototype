module Infer.Compare exposing (compareAnnotation)

import Dict exposing (Dict)
import IR.Annotation as Annotation exposing (Annotation)
import IR.Expr exposing (Expr(..))
import IR.Linearity as Linearity
import IR.Spec as Spec exposing (Spec)



-- TODO: add better mapping to where issue happened


compareAnnotation : Annotation -> Spec -> Maybe String
compareAnnotation annotation spec =
    case compareAnnotationHelp annotation spec empty of
        Ok _ ->
            Nothing

        Err err ->
            Just err


compareAnnotationHelp : Annotation -> Spec -> CompareState -> Result String CompareState
compareAnnotationHelp annotation spec state =
    case annotation of
        Annotation.Reference expectedLinearity name ->
            case spec of
                Spec.Reference gotLinearity address ->
                    if not expectedLinearity || gotLinearity then
                        let
                            ( nameAddress, nextState ) =
                                getNameAddress name state

                            ( isolatedAddress, lastState ) =
                                getIsolatedAddress address nextState
                        in
                        if nameAddress == isolatedAddress then
                            Ok lastState

                        else
                            Err "expected another variable"

                    else
                        Err <| "expected " ++ Debug.toString expectedLinearity ++ " but found " ++ Debug.toString gotLinearity

                Spec.Arrow _ _ _ ->
                    Err "expected reference, got arrow"

        Annotation.Arrow expectedLinearity expectedArgument expectedReturn ->
            case spec of
                Spec.Arrow gotLinearity gotArgument gotReturn ->
                    if expectedLinearity /= Linearity.Linear || gotLinearity == Linearity.Linear then
                        case compareAnnotationHelp expectedArgument gotArgument state of
                            Ok functionState ->
                                case compareAnnotationHelp expectedReturn gotReturn functionState of
                                    Ok returnState ->
                                        Ok returnState

                                    err ->
                                        err

                            err ->
                                err

                    else
                        Err <| "Expected " ++ Debug.toString expectedLinearity ++ " but found " ++ Debug.toString gotLinearity

                Spec.Reference _ _ ->
                    Err "expected arrow, got reference"


getNameAddress : String -> CompareState -> ( Int, CompareState )
getNameAddress name state =
    case Dict.get name state.cache1 of
        Just address ->
            ( address, state )

        Nothing ->
            let
                { count1, cache1 } =
                    state
            in
            ( count1, { state | count1 = count1 + 1, cache1 = Dict.insert name count1 cache1 } )


getIsolatedAddress : Int -> CompareState -> ( Int, CompareState )
getIsolatedAddress address state =
    case Dict.get address state.cache2 of
        Just address_ ->
            ( address_, state )

        Nothing ->
            let
                { count2, cache2 } =
                    state
            in
            ( count2, { state | count2 = count2 + 1, cache2 = Dict.insert address count2 cache2 } )


type alias CompareState =
    { count1 : Int
    , cache1 : Dict String Int
    , count2 : Int
    , cache2 : Dict Int Int
    }


empty =
    { count1 = 0
    , cache1 = Dict.empty
    , count2 = 0
    , cache2 = Dict.empty
    }
