module IR.Parser.Name exposing (..)

import IR.Annotation as Annotation exposing (Annotation)
import IR.Linearity as Linearity exposing (Linearity)
import Parser exposing ((|.), (|=), Parser)


fieldName : Parser String
fieldName =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf (\c -> Char.isLower c || c == '_')
            |. Parser.chompWhile (\c -> Char.isAlphaNum c || c == '_')
