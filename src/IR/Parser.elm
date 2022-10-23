module IR.Parser exposing (..)

import Parser exposing ((|.), (|=), Parser)

lineComment : Parser ()
lineComment =
    Parser.lineComment "#"

multiComment : Parser ()
multiComment =
    Parser.multiComment "###" "###" Parser.NotNestable


var : Parser String
var =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf Char.isLower
            |. Parser.chompWhile (\c -> Char.isAlphaNum c || c == '_')


param : Parser String
param =
    Parser.succeed identity
        |. Parser.keyword "param "
        |. Parser.spaces
        |= var
        |. Parser.spaces
        |. Parser.symbol "="

annotation : Parser a
annotation =
    Debug.todo ""
