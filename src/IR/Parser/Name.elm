module IR.Parser.Name exposing (..)

import Parser exposing ((|.), (|=), Parser)


fieldName : Parser String
fieldName =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf (\c -> Char.isLower c || c == '_')
            |. Parser.chompWhile (\c -> Char.isAlphaNum c || c == '_')
