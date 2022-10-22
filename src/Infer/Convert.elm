module Infer.Convert exposing (convert)

import Dict exposing (Dict)
import Html exposing (address)
import IR.Annotation as Annotation exposing (Annotation)
import IR.Spec as Spec exposing (Spec)
import Infer.Model as Model exposing (Model)


convert : Annotation -> Model -> ( Spec, Model )
convert annotation model =
    let
        ( spec, nextModel, _ ) =
            convertHelp annotation model Dict.empty
    in
    ( spec, nextModel )


convertHelp : Annotation -> Model -> State -> ( Spec, Model, State )
convertHelp annotation model state =
    case annotation of
        Annotation.Reference linearity name ->
            case Dict.get name state of
                Just address ->
                    ( Spec.Reference linearity address
                    , model
                    , state
                    )

                Nothing ->
                    let
                        ( address, nextModel ) =
                            Model.nextFreeSpecAddress model
                    in
                    ( Spec.Reference linearity address
                    , nextModel
                    , Dict.insert name address state
                    )

        Annotation.Arrow linearity argument return ->
            let
                ( argumentSpec, argumentModel, argumentState ) =
                    convertHelp argument model state

                ( returnSpec, returnModel, returnState ) =
                    convertHelp return argumentModel argumentState
            in
            ( Spec.Arrow linearity argumentSpec returnSpec, returnModel, returnState )


type alias State =
    Dict String Int