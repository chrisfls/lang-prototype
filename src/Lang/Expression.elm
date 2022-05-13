module Lang.Expression exposing (Expression(..))

{-|


#

@docs Expression

-}

import Lang.Type exposing (Type)


{-| Translate your expressions to this type in order to be able to perform type inference on them.
The Spy variant has no effect on type inference, but can be used to find the type of a subexpression.
-}
type Expression
    = Literal Type
    | Lambda String (Expression -> Expression)
    | Call Expression Expression
    | Let String Expression Expression
    | Name String
    | Spy Expression Int
