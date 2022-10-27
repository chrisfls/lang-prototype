module Formatter exposing (Entries, format, span, text, wrap)


type Entries
    = Text String
    | Span (Maybe Wrapper) (List Entries)


type alias Wrapper =
    { start : String
    , separator : String
    , end : String
    }


text : String -> Entries
text =
    Text


span : List Entries -> Entries
span =
    Span Nothing


wrap : Wrapper -> List Entries -> Entries
wrap wrapper =
    Span (Just wrapper)


format : Int -> Entries -> Maybe String
format max entries =
    case formatHelp max "" 0 0 entries of
        Just ( string, _ ) ->
            Just string

        _ ->
            Nothing


formatHelp : Int -> String -> Int -> Int -> Entries -> Maybe ( String, Int )
formatHelp max buffer column indent entry =
    case entry of
        Text string ->
            let
                length =
                    String.length string
            in
            if length + column < max then
                Just ( buffer ++ string, column + length )

            else
                Nothing

        Span Nothing xs ->
            case xs of
                fst :: rest1 ->
                    case formatInlineRecur True max buffer column indent [ fst ] of
                        Just ( nextBuffer1, nextColumn1 ) ->
                            case rest1 of
                                snd :: rest2 ->
                                    case formatInlineRecur False max nextBuffer1 nextColumn1 indent [ snd ] of
                                        Just ( nextBuffer2, nextColumn2 ) ->
                                            formatRecur max nextBuffer2 nextColumn2 (indent + 1) rest2

                                        Nothing ->
                                            formatRecur max nextBuffer1 nextColumn1 (indent + 1) rest1

                                [] ->
                                    Just ( nextBuffer1, nextColumn1 )

                        Nothing ->
                            formatRecur max buffer column (indent + 1) xs

                [] ->
                    Just ( buffer, column )

        Span (Just wrapper) xs ->
            case formatWrapInlineRecur True max buffer column indent wrapper xs of
                Just ok ->
                    Just ok

                Nothing ->
                    formatWrapRecur True max buffer (indent + 1) wrapper xs



-- inline


formatInlineRecur : Bool -> Int -> String -> Int -> Int -> List Entries -> Maybe ( String, Int )
formatInlineRecur first max buffer column indent entries =
    case ( first, entries ) of
        ( True, entry :: nextEntries ) ->
            case formatHelp max buffer column indent entry of
                Just ( nextBuffer, nextColumn ) ->
                    formatInlineRecur False max nextBuffer nextColumn indent nextEntries

                nothing ->
                    nothing

        ( False, entry :: nextEntries ) ->
            if column + 1 < max then
                case formatHelp max (buffer ++ " ") (column + 1) indent entry of
                    Just ( nextBuffer, nextColumn2 ) ->
                        formatInlineRecur False max nextBuffer nextColumn2 indent nextEntries

                    nothing ->
                        nothing

            else
                Nothing

        ( _, [] ) ->
            Just ( buffer, column )


formatWrapInlineRecur : Bool -> Int -> String -> Int -> Int -> Wrapper -> List Entries -> Maybe ( String, Int )
formatWrapInlineRecur first max buffer column indent wrapper entries =
    case ( first, entries ) of
        ( True, entry :: nextEntries ) ->
            let
                nextColumn1 =
                    column + String.length wrapper.start
            in
            if nextColumn1 < max then
                case formatHelp max (buffer ++ wrapper.start) nextColumn1 indent entry of
                    Just ( nextBuffer, nextColumn2 ) ->
                        formatWrapInlineRecur False max nextBuffer nextColumn2 indent wrapper nextEntries

                    nothing ->
                        nothing

            else
                Nothing

        ( False, entry :: nextEntries ) ->
            let
                nextColumn1 =
                    column + String.length wrapper.separator
            in
            if nextColumn1 < max then
                case formatHelp max (buffer ++ wrapper.separator) nextColumn1 indent entry of
                    Just ( nextBuffer, nextColumn2 ) ->
                        formatWrapInlineRecur False max nextBuffer nextColumn2 indent wrapper nextEntries

                    nothing ->
                        nothing

            else
                Nothing

        ( True, [] ) ->
            let
                nextColumn =
                    column + String.length wrapper.start + String.length wrapper.end
            in
            if nextColumn < max then
                Just ( buffer ++ wrapper.start ++ wrapper.end, nextColumn )

            else
                Nothing

        ( False, [] ) ->
            let
                nextColumn =
                    column + String.length wrapper.end
            in
            if nextColumn < max then
                Just ( buffer ++ wrapper.end, nextColumn )

            else
                Nothing



-- spread


formatRecur : Int -> String -> Int -> Int -> List Entries -> Maybe ( String, Int )
formatRecur max buffer column indent entries =
    case entries of
        entry :: nextEntries ->
            let
                nextBuffer1 =
                    buffer ++ newline indent

                nextColumn1 =
                    2 * indent
            in
            case formatHelp max nextBuffer1 nextColumn1 indent entry of
                Just ( nextBuffer2, nextColumn2 ) ->
                    formatRecur max nextBuffer2 nextColumn2 indent nextEntries

                nothing ->
                    nothing

        [] ->
            Just ( buffer, column )


formatWrapRecur : Bool -> Int -> String -> Int -> Wrapper -> List Entries -> Maybe ( String, Int )
formatWrapRecur first max buffer indent wrapper entries =
    case entries of
        entry :: nextEntries ->
            let
                prefix =
                    if first then
                        wrapper.start

                    else
                        wrapper.separator

                nextBuffer1 =
                    buffer ++ newline indent ++ prefix

                nextColumn1 =
                    (2 * indent) + String.length prefix
            in
            case formatHelp max nextBuffer1 nextColumn1 indent entry of
                Just ( nextBuffer2, _ ) ->
                    formatWrapRecur False max nextBuffer2 indent wrapper nextEntries

                nothing ->
                    nothing

        [] ->
            let
                nextColumn =
                    (2 * indent) + String.length wrapper.end
            in
            if nextColumn < max then
                Just ( buffer ++ newline indent ++ wrapper.end, nextColumn )

            else
                Nothing


newline : Int -> String
newline indent =
    "\n" ++ String.repeat indent "  "
