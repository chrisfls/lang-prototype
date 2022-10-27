module Formatter exposing (..)


type Entries
    = Entry String
    | Group (Maybe Wrap) (List Entries)


type alias Wrap =
    { start : String
    , separator : String
    , end : String
    }


formatHelp : Int -> String -> Int -> Int -> Entries -> Maybe ( String, Int )
formatHelp max buffer column indent entry =
    case entry of
        Entry text ->
            let
                length =
                    String.length text
            in
            if length + column < max then
                Just ( text, column + length )

            else
                Nothing

        Group Nothing xs ->
            formatInlineRecur True max buffer column indent xs

        Group (Just wrap) xs ->
            formatWrapInlineRecur True max buffer column indent wrap xs


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


formatWrapInlineRecur : Bool -> Int -> String -> Int -> Int -> Wrap -> List Entries -> Maybe ( String, Int )
formatWrapInlineRecur first max buffer column indent wrap entries =
    case ( first, entries ) of
        ( True, entry :: nextEntries ) ->
            let
                nextColumn1 =
                    column + String.length wrap.start
            in
            if nextColumn1 < max then
                case formatHelp max (buffer ++ wrap.start) nextColumn1 indent entry of
                    Just ( nextBuffer, nextColumn2 ) ->
                        formatWrapInlineRecur False max nextBuffer nextColumn2 indent wrap nextEntries

                    nothing ->
                        nothing

            else
                Nothing

        ( False, entry :: nextEntries ) ->
            let
                nextColumn1 =
                    column + String.length wrap.separator
            in
            if nextColumn1 < max then
                case formatHelp max (buffer ++ wrap.separator) nextColumn1 indent entry of
                    Just ( nextBuffer, nextColumn2 ) ->
                        formatWrapInlineRecur False max nextBuffer nextColumn2 indent wrap nextEntries

                    nothing ->
                        nothing

            else
                Nothing

        ( _, [] ) ->
            let
                nextColumn =
                    column + String.length wrap.end
            in
            if nextColumn < max then
                Just ( buffer ++ wrap.end, nextColumn )

            else
                Nothing


formatRecur : Bool -> Int -> String -> Int -> Int -> List Entries -> Maybe ( String, Int )
formatRecur first max buffer column indent entries =
    case ( first, entries ) of
        ( True, entry :: nextEntries ) ->
            case formatHelp max buffer column indent entry of
                Just ( nextBuffer, nextColumn ) ->
                    formatRecur False max nextBuffer nextColumn indent nextEntries

                Nothing ->
                    formatRecurHelp max buffer indent entry nextEntries

        ( False, entry :: nextEntries ) ->
            formatRecurHelp max buffer indent entry nextEntries

        ( _, [] ) ->
            Just ( buffer, column )


formatRecurHelp : Int -> String -> Int -> Entries -> List Entries -> Maybe ( String, Int )
formatRecurHelp max buffer indent entry entries =
    let
        nextBuffer1 =
            buffer ++ newline indent

        nextColumn =
            2 * indent
    in
    case formatHelp max nextBuffer1 nextColumn indent entry of
        Just ( nextBuffer2, nextColumn2 ) ->
            case entries of
                entry_ :: entries_ ->
                    formatRecurHelp max nextBuffer2 indent entry_ entries_

                [] ->
                    Just ( nextBuffer2, nextColumn2 )

        nothing ->
            nothing


formatWrapRecur : Bool -> Int -> String -> Int -> Wrap -> List Entries -> Maybe ( String, Int )
formatWrapRecur first max buffer indent wrap entries =
    case entries of
        entry :: nextEntries ->
            let
                wrapper =
                    if first then
                        wrap.start

                    else
                        wrap.separator

                nextBuffer1 =
                    buffer ++ newline indent ++ wrapper

                nextColumn1 =
                    (2 * indent) + String.length wrapper
            in
            case formatHelp max nextBuffer1 nextColumn1 indent entry of
                Just ( nextBuffer2, _ ) ->
                    formatWrapRecur False max nextBuffer2 indent wrap nextEntries

                nothing ->
                    nothing

        [] ->
            let
                nextColumn =
                    (2 * indent) + String.length wrap.end
            in
            if nextColumn < max then
                Just ( buffer ++ newline indent ++ wrap.end, nextColumn )

            else
                Nothing


newline : Int -> String
newline indent =
    "\n" ++ String.repeat indent "  "
