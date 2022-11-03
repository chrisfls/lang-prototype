module PrettyPrint exposing
    ( Element, text, break, group, span, indent
    , comma, semicolon, colon, whitespace, always
    , Condition, onNeed, onSpan
    , OrElse, or, orNoop
    , toString, stringify
    )

{-|


# Build


## Primitives

@docs Element, text, break, group, span, indent


## Common

@docs comma, semicolon, colon, whitespace, always


## Conditions

@docs Condition, onNeed, onSpan


## OrElse

@docs OrElse, or, orNoop


# Format

@docs toString, stringify

-}


type Element
    = Text String
    | Break Condition
    | Group (List Element)
    | Span (List Element)
    | Indent Int Element


type Condition
    = Always
    | OnNeed OrElse
    | OnSpan OrElse


type OrElse
    = OrElement Element
    | OrNoop



-- Primitives


{-| Plain text.
-}
text : String -> Element
text =
    Text


{-| Line break depending on some condition.
-}
break : Condition -> Element
break =
    Break


{-| A group of `Element` that has no influence over break.
-}
group : List Element -> Element
group =
    Group


{-| A group of `Element` that is influenced by `OnSpan` break.
-}
span : List Element -> Element
span =
    Span


{-| Controls indent level of `Element`, only goes into effect if element has breaks.
-}
indent : Int -> Element -> Element
indent =
    Indent



-- Common


{-| A comma with space afterwards.
-}
comma : Element
comma =
    text ", "


{-| A plain semicolon.
-}
semicolon : Element
semicolon =
    text ";"


{-| A plain colon.
-}
colon : Element
colon =
    text ":"


{-| A single white space.
-}
whitespace : Element
whitespace =
    text " "



-- Conditions


{-| Always break.
-}
always : Condition
always =
    Always


{-| Break if no space is available for next element in the group or span.
-}
onNeed : OrElse -> Condition
onNeed =
    OnNeed


{-| Break if any part of the parent span will have to be broken.
-}
onSpan : OrElse -> Condition
onSpan =
    OnSpan



-- OrElse


{-| If break is not done, embed this Element instead.
-}
or : Element -> OrElse
or =
    OrElement


{-| Noop if `Break` is not done.
-}
orNoop : OrElse
orNoop =
    OrNoop



-- Format


toString : Element -> String
toString =
    stringify 80


stringify : Int -> Element -> String
stringify width element =
    (item 0
        False
        element
        { buffer = ""
        , column = 0
        , width = width
        }
    ).buffer



-- internals


type alias State =
    { buffer : String
    , column : Int
    , width : Int
    }


item : Int -> Bool -> Element -> State -> State
item offset brokenSpan element state =
    case element of
        Text string ->
            write string state

        Break _ ->
            state

        Group elements ->
            list offset brokenSpan elements state

        Span elements ->
            list offset (isListBroken elements state) elements state

        Indent diff subElement ->
            if isElementBroken subElement state then
                item offset brokenSpan subElement state

            else
                item (offset + diff) brokenSpan subElement state


list : Int -> Bool -> List Element -> State -> State
list offset brokenSpan elements state =
    case elements of
        head :: tail ->
            case head of
                Break Always ->
                    breakAndContinue offset brokenSpan tail state

                Break (OnNeed condition) ->
                    if isWholeTillBreak tail state then
                        embedAndContinue offset brokenSpan condition tail state

                    else
                        breakAndContinue offset brokenSpan tail state

                Break (OnSpan condition) ->
                    if brokenSpan then
                        breakAndContinue offset True tail state

                    else
                        embedAndContinue offset brokenSpan condition tail state

                _ ->
                    continue offset brokenSpan head tail state

        [] ->
            state


continue : Int -> Bool -> Element -> List Element -> State -> State
continue offset brokenSpan head tail state =
    item offset brokenSpan head state
        |> list offset brokenSpan tail


breakAndContinue : Int -> Bool -> List Element -> State -> State
breakAndContinue offset brokenSpan xs state =
    linebreak offset state
        |> list offset brokenSpan xs


embedAndContinue : Int -> Bool -> OrElse -> List Element -> State -> State
embedAndContinue offset brokenSpan condition xs state =
    case condition of
        OrElement element ->
            item offset brokenSpan element state
                |> list offset brokenSpan xs

        OrNoop ->
            list offset brokenSpan xs state


write : String -> State -> State
write string state =
    { buffer = state.buffer ++ string
    , column = state.column + String.length string
    , width = state.width
    }


linebreak : Int -> State -> State
linebreak column state =
    { buffer = String.trimRight state.buffer ++ "\n" ++ String.repeat column " "
    , column = column
    , width = state.width
    }


isElementBroken : Element -> State -> Bool
isElementBroken element state =
    case element of
        Text string ->
            state.column + String.length string <= state.width

        Break Always ->
            False

        Break _ ->
            state.column <= state.width

        Group elements ->
            isListBroken elements state

        Span elements ->
            isListBroken elements state

        Indent _ subElement ->
            isElementBroken subElement state


isListBroken : List Element -> State -> Bool
isListBroken elements state =
    case getUnbrokenListColumn state.width state.column elements of
        Just _ ->
            True

        Nothing ->
            False


getUnbrokenListColumn : Int -> Int -> List Element -> Maybe Int
getUnbrokenListColumn width column elements =
    if column <= width then
        case elements of
            [] ->
                Just column

            head :: tail ->
                case getUnbrokenElementColumn width column head of
                    Just column_ ->
                        getUnbrokenListColumn width column_ tail

                    nothing ->
                        nothing

    else
        Nothing


getUnbrokenElementColumn : Int -> Int -> Element -> Maybe Int
getUnbrokenElementColumn width column element =
    case element of
        Text string ->
            let
                column_ =
                    column + String.length string
            in
            if column_ <= width then
                Just column_

            else
                Nothing

        Break Always ->
            Nothing

        Break (OnNeed (OrElement subElement)) ->
            getUnbrokenElementColumn width column subElement

        Break (OnSpan (OrElement subElement)) ->
            getUnbrokenElementColumn width column subElement

        Break _ ->
            Just column

        Group elements ->
            getUnbrokenListColumn width column elements

        Span elements ->
            getUnbrokenListColumn width column elements

        Indent _ subElement ->
            getUnbrokenElementColumn width column subElement


isWholeTillBreak : List Element -> State -> Bool
isWholeTillBreak elements state =
    case getWholeListColumn state.width state.column elements of
        Just _ ->
            True

        Nothing ->
            False


getWholeListColumn : Int -> Int -> List Element -> Maybe ( Bool, Int )
getWholeListColumn width column elements =
    if column <= width then
        case elements of
            [] ->
                Just ( True, column )

            head :: tail ->
                case getWholeElementColumn width column head of
                    Just ( True, column_ ) ->
                        getWholeListColumn width column_ tail

                    halt ->
                        halt

    else
        Nothing


getWholeElementColumn : Int -> Int -> Element -> Maybe ( Bool, Int )
getWholeElementColumn width column element =
    case element of
        Text string ->
            let
                column_ =
                    column + String.length string
            in
            if column_ <= width then
                Just ( True, column_ )

            else
                Nothing

        Break _ ->
            -- halt on first break
            Just ( False, column )

        Group subElements ->
            getWholeListColumn width column subElements

        Span subElements ->
            getWholeListColumn width column subElements

        Indent _ subElement ->
            getWholeElementColumn width column subElement
