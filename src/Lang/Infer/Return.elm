module Lang.Infer.Return exposing (Return(..))

import Lang.Canonical.Type exposing (Type)
import Lang.Infer.Error exposing (Error(..))
import Lang.Infer.State exposing (State)


type Return
    = Throw Error
    | Return Type State
