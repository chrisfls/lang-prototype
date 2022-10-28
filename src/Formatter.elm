module Formatter exposing (Entries, format, span, text, wrap)


type Entries
    = Text String
    | Span (List Entries)
    | Wrap Wrapper (List Entries)


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
    Span


wrap : Wrapper -> List Entries -> Entries
wrap =
    Wrap


format : Int -> Entries -> Maybe String
format max entries =
    case fit max "" 0 0 entries of
        Just ( string, _ ) ->
            Just string

        _ ->
            Nothing


fit : Int -> String -> Int -> Int -> Entries -> Maybe ( String, Int )
fit max buffer column level entry =
    case entry of
        Text _ ->
            case inline max buffer column entry of
                Nothing ->
                    spread max buffer column (level + 1) entry

                just ->
                    just

        Span entries ->
            case entries of
                fst :: fstEntries ->
                    -- try to inline first element
                    case inline max buffer column fst of
                        (Just ( fstBuffer, fstColumn )) as fstJust ->
                            case fstEntries of
                                snd :: sndEntries ->
                                    -- try to inline second element
                                    case inline max fstBuffer fstColumn snd of
                                        Just ( sndBuffer, sndColumn ) ->
                                            -- try to inline the rest after the second element
                                            case inlineSpan max sndBuffer sndColumn sndEntries of
                                                Nothing ->
                                                    spreadSpan max sndBuffer sndColumn (level + 1) sndEntries

                                                just ->
                                                    just

                                        Nothing ->
                                            -- no space to inline the second element, try to spread the tail of the first element
                                            spreadSpan max fstBuffer fstColumn (level + 1) fstEntries

                                [] ->
                                    -- there's no tail after the first element
                                    fstJust

                        Nothing ->
                            -- no space to inline the first element, try to spread everything
                            spreadSpan max buffer column (level + 1) entries

                [] ->
                    case inline max buffer column entry of
                        Nothing ->
                            spreadSpan max buffer column (level + 1) entries

                        just ->
                            just

        Wrap wrapper entries ->
            case inlineWrap True max buffer level wrapper entries of
                Nothing ->
                    spreadWrap True max buffer (level + 1) wrapper entries

                just ->
                    just



-- inline


inline : Int -> String -> Int -> Entries -> Maybe ( String, Int )
inline max buffer column entry =
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

        Span entries ->
            inlineSpan max buffer column entries

        Wrap wrapper entries ->
            inlineWrap True max buffer column wrapper entries


inlineSpan : Int -> String -> Int -> List Entries -> Maybe ( String, Int )
inlineSpan max buffer column entries =
    case entries of
        entry :: nextEntries ->
            case inline max buffer column entry of
                Just ( nextBuffer, nextColumn ) ->
                    if List.isEmpty nextEntries then
                        Just ( nextBuffer, nextColumn )

                    else if nextColumn + 1 < max then
                        inlineSpan max (nextBuffer ++ " ") (nextColumn + 1) nextEntries

                    else
                        Nothing

                nothing ->
                    nothing

        [] ->
            Just ( buffer, column )


inlineWrap : Bool -> Int -> String -> Int -> Wrapper -> List Entries -> Maybe ( String, Int )
inlineWrap first max buffer column wrapper entries =
    case entries of
        entry :: nextEntries ->
            let
                prefix =
                    if first then
                        wrapper.start

                    else
                        wrapper.separator

                probColumn =
                    column + String.length prefix + 1
            in
            if probColumn < max then
                case inline max (buffer ++ prefix ++ " ") probColumn entry of
                    Just ( nextBuffer, nextColumn ) ->
                        inlineWrap False max nextBuffer nextColumn wrapper nextEntries

                    nothing ->
                        nothing

            else
                Nothing

        [] ->
            let
                string =
                    if first then
                        wrapper.start ++ wrapper.end

                    else
                        " " ++ wrapper.end

                nextColumn =
                    column + String.length string
            in
            if nextColumn < max then
                Just ( buffer ++ string, nextColumn )

            else
                Nothing



-- spread


spread : Int -> String -> Int -> Int -> Entries -> Maybe ( String, Int )
spread max buffer column level entry =
    case entry of
        Text string ->
            let
                ( indentBuffer, indentColumn ) =
                    indent level buffer
            in
            Just ( indentBuffer ++ string, indentColumn + String.length string )

        Span entries ->
            spreadSpan max buffer column level entries

        Wrap wrapper entries ->
            spreadWrap True max buffer level wrapper entries


spreadSpan : Int -> String -> Int -> Int -> List Entries -> Maybe ( String, Int )
spreadSpan max buffer column level entries =
    case entries of
        entry :: nextEntries ->
            let
                ( indentBuffer, indentColumn ) =
                    indent level buffer
            in
            case fit max indentBuffer indentColumn level entry of
                Just ( nextBuffer, nextColumn ) ->
                    spreadSpan max nextBuffer nextColumn level nextEntries

                nothing ->
                    nothing

        [] ->
            Just ( buffer, column )


spreadWrap : Bool -> Int -> String -> Int -> Wrapper -> List Entries -> Maybe ( String, Int )
spreadWrap first max buffer level wrapper entries =
    let
        ( indentBuffer, indentColumn ) =
            indent level buffer
    in
    case entries of
        entry :: nextEntries ->
            let
                prefix =
                    if first then
                        wrapper.start

                    else
                        wrapper.separator

                probColumn =
                    indentColumn + String.length prefix + 1
            in
            case inline max (indentBuffer ++ prefix ++ " ") probColumn entry of
                Just ( nextBuffer, nextColumn ) ->
                    inlineWrap False max nextBuffer nextColumn wrapper nextEntries

                Nothing ->
                    case spread max (indentBuffer ++ prefix ++ " ") probColumn level entry of
                        Just ( nextBuffer, _ ) ->
                            spreadWrap False max nextBuffer level wrapper nextEntries

                        nothing ->
                            nothing

        [] ->
            let
                suffix =
                    if first then
                        wrapper.start ++ wrapper.end

                    else
                        wrapper.end

                nextColumn =
                    indentColumn + String.length suffix
            in
            if nextColumn < max then
                Just ( indentBuffer ++ suffix, nextColumn )

            else
                Nothing


indent : Int -> String -> ( String, Int )
indent level buffer =
    let
        padding =
            String.repeat level "  "
    in
    if buffer == "" then
        ( padding, String.length padding )

    else
        ( buffer ++ "\n" ++ padding, String.length padding )
