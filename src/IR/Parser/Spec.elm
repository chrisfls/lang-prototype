module IR.Parser.Spec exposing (..)

import IR.Annotation as Annotation exposing (Annotation)
import IR.Linearity as Linearity exposing (Linearity)
import Parser exposing ((|.), (|=), Parser)


annotation : Parser Annotation
annotation =
    Parser.andThen
        (\argm -> Parser.map (toArrow argm) arrowList)
        annotationWithoutArrow


toArrow : Annotation -> List ( Linearity, Annotation ) -> Annotation
toArrow left xs =
    case xs of
        [] ->
            left

        [ ( linearity, right ) ] ->
            Annotation.Arrow linearity left right

        ( linearity, right ) :: rest ->
            Annotation.Arrow linearity left (toArrowHelp right rest)


toArrowHelp : Annotation -> List ( Linearity, Annotation ) -> Annotation
toArrowHelp right entries =
    case entries of
        ( linearity, left ) :: rest ->
            toArrowHelp (Annotation.Arrow linearity left right) rest

        [] ->
            right


arrowList : Parser (List ( Linearity, Annotation ))
arrowList =
    Parser.loop [] arrowListHelp


arrowListHelp : List ( Linearity, Annotation ) -> Parser (Parser.Step (List ( Linearity, Annotation )) (List ( Linearity, Annotation )))
arrowListHelp xs =
    Parser.oneOf
        [ Parser.succeed (\linearity right -> Parser.Loop (( linearity, right ) :: xs))
            |. Parser.spaces
            |= arrowLinearity
            |. Parser.spaces
            |= annotationWithoutArrow
        , Parser.succeed (Parser.Done xs)
        ]


annotationWithoutArrow : Parser Annotation
annotationWithoutArrow =
    Parser.oneOf
        [ linearReference
        , varyingReference
        , tuple
        ]


tuple : Parser Annotation
tuple =
    Parser.sequence
        { start = "("
        , separator = ","
        , end = ")"
        , spaces = Parser.spaces
        , item = Parser.lazy (\_ -> annotation)
        , trailing = Parser.Forbidden
        }
        |> Parser.map
            (\entries ->
                case entries of
                    [] ->
                        Annotation.Unit

                    [ spec ] ->
                        spec

                    _ ->
                        Annotation.Tuple entries
            )


varyingReference : Parser Annotation
varyingReference =
    Parser.succeed (Annotation.Reference False)
        |= variableName


linearReference : Parser Annotation
linearReference =
    Parser.succeed (Annotation.Reference True)
        |. Parser.symbol "*"
        |. Parser.spaces
        |= variableName


variableName : Parser String
variableName =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf (\c -> Char.isLower c)
            |. Parser.chompWhile (\c -> Char.isAlphaNum c)
            |. Parser.chompWhile (\c -> c == '\'')



-- internals


arrowLinearity : Parser Linearity
arrowLinearity =
    Parser.oneOf
        [ Parser.succeed Linearity.Varying
            |. Parser.symbol "->"
        , Parser.succeed Linearity.Closure
            |. Parser.symbol "=>"
        ]
