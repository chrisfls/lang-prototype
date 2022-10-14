module IR.Annotation exposing (Annotation(..))

import IR.Linearity exposing (Linearity)



-- TODO: start sourcemaps


type Annotation
    = Reference Bool String
    | Arrow Linearity Annotation Annotation
