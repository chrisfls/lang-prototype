module IR.Parser.Spec exposing (..)

import Dict exposing (Dict)
import IR.Annotation as Annotation exposing (Annotation)
import IR.Linearity as Linearity exposing (Linearity)
import IR.Parser.Name as Name
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
    Parser.succeed identity
        |. Parser.spaces
        |= Parser.loop [] arrowListHelp


arrowListHelp : List ( Linearity, Annotation ) -> Parser (Parser.Step (List ( Linearity, Annotation )) (List ( Linearity, Annotation )))
arrowListHelp xs =
    Parser.oneOf
        [ Parser.succeed (\linearity right -> Parser.Loop (( linearity, right ) :: xs))
            |= arrowLinearity
            |. Parser.spaces
            |= annotationWithoutArrow
            |. Parser.spaces
        , Parser.succeed (Parser.Done xs)
        ]


annotationWithoutArrow : Parser Annotation
annotationWithoutArrow =
    Parser.oneOf
        [ linearReference
        , varyingReference
        , tuple
        , record
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


type Record
    = EmptyRecord
    | ExtensibleRecord String
    | PlainRecord String


record : Parser Annotation
record =
    Parser.succeed identity
        |. Parser.symbol "{"
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed EmptyRecord
                |. Parser.symbol "}"
            , Parser.succeed (\name fn -> fn name)
                |= Name.fieldName
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed ExtensibleRecord
                        |. Parser.symbol "|"
                    , Parser.succeed PlainRecord
                        |. Parser.symbol ":"
                    ]
            ]
        |> Parser.andThen
            (\shape ->
                case shape of
                    EmptyRecord ->
                        Parser.succeed (Annotation.Record Nothing Dict.empty)

                    ExtensibleRecord name ->
                        Parser.succeed (\field spec dict -> Annotation.Record (Just name) (Dict.insert field spec dict))
                            |. Parser.spaces
                            |= Name.fieldName
                            |. Parser.spaces
                            |. Parser.symbol ":"
                            |. Parser.spaces
                            |= Parser.lazy (\_ -> annotation)
                            |. Parser.spaces
                            |. Parser.spaces
                            |= recordDict

                    PlainRecord name ->
                        Parser.succeed (\spec dict -> Annotation.Record Nothing (Dict.insert name spec dict))
                            |. Parser.spaces
                            |= Parser.lazy (\_ -> annotation)
                            |. Parser.spaces
                            |= recordDict
            )


recordDict : Parser (Dict String Annotation)
recordDict =
    Parser.loop Dict.empty recordDictHelp


recordDictHelp : Dict String Annotation -> Parser (Parser.Step (Dict String Annotation) (Dict String Annotation))
recordDictHelp dict =
    Parser.oneOf
        [ Parser.succeed (\name spec -> Parser.Loop (Dict.insert name spec dict))
            |. Parser.symbol ","
            |. Parser.spaces
            |= Name.fieldName
            |. Parser.spaces
            |. Parser.symbol ":"
            |. Parser.spaces
            |= Parser.lazy (\_ -> annotation)
            |. Parser.spaces
        , Parser.succeed (Parser.Done dict)
            |. Parser.symbol "}"
        ]


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





-- dot : Parser Annotation
-- dot =
--     Parser.andThen
--         (\argm -> Parser.map (toArrow argm) arrowList)
--         specReference


-- dotAccess : Annotation -> List ( Linearity, Annotation ) -> Annotation
-- dotAccess left xs =
--     case xs of
--         [] ->
--             left

--         [ right ] ->
--             Annotation.DotAccess left right

--         ( linearity, right ) :: rest ->
--             Annotation.Arrow linearity left (toArrowHelp right rest)


-- dotAccessHelp : Annotation -> List ( Linearity, Annotation ) -> Annotation
-- dotAccessHelp right entries =
--     case entries of
--         ( linearity, left ) :: rest ->
--             toArrowHelp (Annotation.Arrow linearity left right) rest

--         [] ->
--             right

-- dotAccess: Parser Annotation
-- dotAccess =
--     Parser.succeed Tuple.pair
--         |= capitalizedName
--         |= Parser.oneOf
--             [ Parser.succeed True
--                 |. Parser.symbol "."
--             , Parser.succeed False
--                 |. Parser.spaces
--             ]
--         |> Parser.andThen



-- dotAccessHelp1 : Annotation -> Parser Annotation
-- dotAccessHelp1 start =
--     Parser.loop start dotAccessHelp2


-- dotAccessHelp2 : Annotation -> Parser (Parser.Step Annotation Annotation)
-- dotAccessHelp2 left =
--     Parser.oneOf
--         [ Parser.succeed (\right -> Parser.Loop (Annotation.DotAccess left right))
--             |. Parser.symbol "."
--             |. Parser.spaces
--             |= variableName
--             |. Parser.spaces
--         , Parser.succeed (Parser.Done left)
--         ]



variableName : Parser String
variableName =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf (\c -> Char.isLower c)
            |. Parser.chompWhile (\c -> Char.isAlphaNum c)
            |. Parser.chompWhile (\c -> c == '\'')


capitalizedName : Parser String
capitalizedName =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompIf (\c -> Char.isUpper c)
            |. Parser.chompWhile (\c -> Char.isAlphaNum c)



-- internals


arrowLinearity : Parser Linearity
arrowLinearity =
    Parser.oneOf
        [ Parser.succeed Linearity.Varying
            |. Parser.symbol "->"
        , Parser.succeed Linearity.Closure
            |. Parser.symbol "=>"
        ]
