module IR.Annotation exposing (Annotation(..))

import IR.Linearity exposing (Linearity)



-- TODO: start sourcemaps
-- TODO: add struct {} for module format assert
-- TODO: add module() for exposing assert
-- TODO: add type holes
--

type Annotation
    = Reference Bool String
    | Arrow Linearity Annotation Annotation

