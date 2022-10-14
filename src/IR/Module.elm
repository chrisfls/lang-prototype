module IR.Module exposing (Module(..))

import IR.Annotation exposing (Annotation)


type Module
    = Variable String
    | Lambda String Module
    | Apply Module Module
    | Annotation Annotation
