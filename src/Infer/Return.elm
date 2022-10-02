module Infer.Return exposing (..)

import IR.Spec exposing (Spec)
import Infer.State exposing (State)


type Return
    = Throw Error
    | Return Spec State


type alias Error =
    String
