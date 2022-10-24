module IR.Parser.Expr exposing (..)

import IR.Annotation as Annotation exposing (Annotation)
import IR.Linearity as Linearity exposing (Linearity)
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
            |. Parser.chompIf isVarStart
            |. Parser.chompWhile isVarMiddle
            |. Parser.chompWhile isVarEnd


isVarStart : Char -> Bool
isVarStart char =
    Char.isLower char || char == '_'


isVarMiddle : Char -> Bool
isVarMiddle char =
    Char.isAlphaNum char || char == '_'


isVarEnd : Char -> Bool
isVarEnd char =
    char == '\''


param : Parser String
param =
    Parser.succeed identity
        |. Parser.keyword "param"
        |. Parser.spaces
        |= var
