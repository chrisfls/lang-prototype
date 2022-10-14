module IR.Annotation exposing (Annotation(..))

import IR.Linearity exposing (Linearity)


type Annotation
    = Reference Bool String
    | Arrow Linearity Annotation Annotation
