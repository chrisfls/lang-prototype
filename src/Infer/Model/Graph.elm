module Infer.Model.Graph exposing (Graph, empty, get, insert, nextFreeAddress, toIntDict)

import IR.Spec exposing (Spec(..))
import IntDict exposing (IntDict)


type alias Graph =
    { store : IntDict Spec
    , count : Int
    }


empty : Graph
empty =
    { store = IntDict.empty
    , count = 0
    }


insert : Int -> Spec -> Graph -> Graph
insert address spec graph =
    { store = IntDict.insert address spec graph.store
    , count = graph.count
    }


get : Int -> Graph -> Maybe Spec
get address graph =
    getHelp address graph.store


nextFreeAddress : Graph -> ( Int, Graph )
nextFreeAddress { store, count } =
    ( count
    , { store = store
      , count = count + 1
      }
    )


toIntDict : Graph -> IntDict Spec
toIntDict { store } =
    store



-- internals


getHelp : Int -> IntDict Spec -> Maybe Spec
getHelp address store =
    case IntDict.get address store of
        (Just spec) as justSpec ->
            case spec of
                Reference _ nextAddress ->
                    case getHelp nextAddress store of
                        (Just _) as justSpec_ ->
                            justSpec_

                        Nothing ->
                            justSpec

                _ ->
                    justSpec

        nothing ->
            nothing
