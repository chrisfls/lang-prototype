module Format2.Test exposing (suite)

import Expect
import PrettyPrint as PP exposing (Element)
import Test exposing (..)


suite : Test
suite =
    describe "PrettyPrint"
        [ primitives
        , extensions
        ]


primitives : Test
primitives =
    describe "primitives"
        [ test "text" <|
            \_ ->
                PP.text "a"
                    |> expectPP "a"
        , test "empty span" <|
            \_ ->
                PP.span []
                    |> expectPP ""
        , test "1-elem span" <|
            \_ ->
                PP.span [ PP.text "a" ]
                    |> expectPP "a"
        , test "2-elem span" <|
            \_ ->
                PP.span [ PP.text "a", PP.text "b" ]
                    |> expectPP "ab"
        , test "3-elem span" <|
            \_ ->
                PP.span [ PP.text "a", PP.text "b", PP.text "c" ]
                    |> expectPP "abc"
        , test "1-depth span" <|
            \_ ->
                PP.span [ PP.text "a" ]
                    |> expectPP "a"
        , test "2-depth span" <|
            \_ ->
                PP.span [ PP.text "a", PP.span [ PP.text "b" ] ]
                    |> expectPP "ab"
        , test "3-depth span" <|
            \_ ->
                PP.span [ PP.text "a", PP.span [ PP.text "b", PP.span [ PP.text "c" ] ] ]
                    |> expectPP "abc"
        , test "1-depth span indent" <|
            \_ ->
                PP.span
                    [ PP.break PP.always
                    , PP.text "a"
                    ]
                    |> PP.indent 2
                    |> expectPP "\n  a"
        , test "2-depth span indent" <|
            \_ ->
                PP.span
                    [ PP.break PP.always
                    , PP.text "a"
                    , PP.span
                        [ PP.break PP.always
                        , PP.text "b"
                        , PP.break PP.always
                        , PP.text "c"
                        ]
                        |> PP.indent 2
                    , PP.break PP.always
                    , PP.text "d"
                    ]
                    |> PP.indent 2
                    |> expectPP "\n  a\n    b\n    c\n  d"
        , test "3-depth span indent" <|
            \_ ->
                PP.span
                    [ PP.break PP.always
                    , PP.text "a"
                    , PP.span
                        [ PP.break PP.always
                        , PP.text "b"
                        , PP.span
                            [ PP.break PP.always
                            , PP.text "c"
                            ]
                            |> PP.indent 2
                        , PP.break PP.always
                        , PP.text "d"
                        ]
                        |> PP.indent 2
                    , PP.break PP.always
                    , PP.text "e"
                    ]
                    |> PP.indent 2
                    |> expectPP "\n  a\n    b\n      c\n    d\n  e"
        , test "forced break" <|
            \_ ->
                PP.span [ PP.text "a", PP.break PP.always, PP.text "b" ]
                    |> expectPP "a\nb"
        , test "forced break indent" <|
            \_ ->
                PP.indent 2 (PP.span [ PP.text "a", PP.break PP.always, PP.text "b" ])
                    |> expectPP "a\n  b"
        , test "useless optional break" <|
            \_ ->
                PP.span [ PP.text "a", PP.break (PP.onNeed PP.orNoop), PP.text "b" ]
                    |> expectPP "ab"
        , test "useless continuation break" <|
            \_ ->
                PP.span [ PP.text "a", PP.break (PP.onSpan PP.orNoop), PP.text "b" ]
                    |> expectPP "ab"
        , test "useful optional break" <|
            \_ ->
                PP.span
                    [ PP.text "1abcdef"
                    , PP.break (PP.onNeed PP.orNoop)
                    , PP.text "2abcdef"
                    , PP.break (PP.onNeed PP.orNoop)
                    , PP.text "3abcdef"
                    , PP.break (PP.onNeed PP.orNoop)
                    , PP.text "4abcdef"
                    ]
                    |> expectPP "1abcdef\n2abcdef\n3abcdef\n4abcdef"
        , test "useful span break" <|
            \_ ->
                PP.span
                    [ PP.text "1abc"
                    , PP.break PP.always
                    , PP.text "2abc"
                    , PP.break (PP.onSpan PP.orNoop)
                    , PP.text "3abc"
                    , PP.break (PP.onSpan PP.orNoop)
                    , PP.text "4abc"
                    ]
                    |> expectPP "1abc\n2abc\n3abc\n4abc"
        ]


extensions : Test
extensions =
    describe "extensions"
        [ test "1-element args" <|
            \_ ->
                args [ PP.text "a" ]
                    |> expectPP "a"
        , test "2-element args" <|
            \_ ->
                args [ PP.text "a", PP.text "b" ]
                    |> expectPP "a b"
        , test "3-element args" <|
            \_ ->
                args [ PP.text "a", PP.text "b", PP.text "c" ]
                    |> expectPP "a b c"
        , test "1-depth args" <|
            \_ ->
                args
                    [ PP.text "Lorem.ip"
                    , PP.text "a"
                    , PP.text "b"
                    , PP.text "c"
                    ]
                    |> expectPP
                        "Lorem.ip a\n  b\n  c"
        , test "2-depth args" <|
            \_ ->
                args
                    [ PP.text "Lor.em"
                    , PP.text "a"
                    , PP.text "b"
                    , args
                        [ PP.text "yuck"
                        , PP.text "c"
                        , PP.text "d"
                        , PP.text "e"
                        , PP.text "f"
                        ]
                    , PP.text "g"
                    ]
                    |> expectPP "Lor.em a\n  b\n  yuck c\n    d\n    e\n    f\n  g"
        , test "3-depth args" <|
            \_ ->
                args
                    [ PP.text "Lor.em"
                    , PP.text "a"
                    , args
                        [ PP.text "yuck"
                        , PP.text "b"
                        , args
                            [ PP.text "puck"
                            , PP.text "c"
                            , PP.text "d"
                            , PP.text "e"
                            , PP.text "f"
                            ]
                        , PP.text "g"
                        ]
                    , PP.text "h"
                    ]
                    |> expectPP
                        "Lor.em a\n  yuck b\n    puck c\n      d\n      e\n      f\n    g\n  h"
        , test "empty apply" <|
            \_ ->
                apply []
                    |> expectPP "()"
        , test "1-element apply" <|
            \_ ->
                apply [ PP.text "a" ]
                    |> expectPP "(a)"
        , test "2-element apply" <|
            \_ ->
                apply [ PP.text "a", PP.text "b" ]
                    |> expectPP "(a b)"
        , test "3-element apply" <|
            \_ ->
                apply [ PP.text "a", PP.text "b", PP.text "c" ]
                    |> expectPP "(a b c)"
        , test "1-depth apply" <|
            \_ ->
                apply
                    [ PP.text "Lor.em"
                    , PP.text "a"
                    , PP.text "b"
                    , PP.text "c"
                    , PP.text "d"
                    ]
                    |> expectPP "(Lor.em a\n  b\n  c\n  d\n)"
        , test "2-depth apply" <|
            \_ ->
                apply
                    [ PP.text "Lor.em"
                    , PP.text "a"
                    , PP.text "b"
                    , apply
                        [ PP.text "yuck"
                        , PP.text "c"
                        , PP.text "d"
                        , PP.text "e"
                        , PP.text "f"
                        ]
                    , PP.text "g"
                    ]
                    |> expectPP "(Lor.em a\n  b\n  (yuck c\n    d\n    e\n    f\n  )\n  g\n)"
        , test "3-depth apply" <|
            \_ ->
                apply
                    [ PP.text "Lor.em"
                    , PP.text "a"
                    , apply
                        [ PP.text "yuck"
                        , PP.text "b"
                        , apply
                            [ PP.text "puck"
                            , PP.text "c"
                            , PP.text "d"
                            , PP.text "e"
                            , PP.text "f"
                            ]
                        , PP.text "g"
                        ]
                    , PP.text "h"
                    ]
                    |> expectPP "(Lor.em a\n  (yuck b\n    (puck c\n      d\n      e\n      f\n    )\n    g\n  )\n  h\n)"
        , test "empty brackets" <|
            \_ ->
                brackets []
                    |> expectPP "[]"
        , test "1-element brackets" <|
            \_ ->
                brackets [ PP.text "a" ]
                    |> expectPP "[ a ]"
        , test "2-element brackets" <|
            \_ ->
                brackets
                    [ PP.text "a"
                    , PP.text "b"
                    ]
                    |> expectPP "[ a, b ]"
        , test "32-element brackets" <|
            \_ ->
                brackets
                    [ PP.text "a"
                    , PP.text "b"
                    , PP.text "c"
                    ]
                    |> expectPP "[ a, b, c ]"
        , test "1-depth brackets" <|
            \_ ->
                brackets
                    [ PP.text "a"
                    , PP.text "b"
                    , PP.text "c"
                    , PP.text "d"
                    ]
                    |> expectPP "[ a\n, b\n, c\n, d\n]"
        , test "2-depth brackets" <|
            \_ ->
                brackets
                    [ PP.text "a"
                    , brackets
                        [ PP.text "b"
                        , PP.text "c"
                        , PP.text "d"
                        ]
                    , PP.text "e"
                    , brackets [ PP.text "f" ]
                    , brackets []
                    ]
                    |> expectPP "[ a\n, [ b\n  , c\n  , d\n  ]\n, e\n, [ f ]\n, []\n]"
        , test "3-depth brackets" <|
            \_ ->
                brackets
                    [ PP.text "a"
                    , brackets
                        [ PP.text "b"
                        , brackets [ PP.text "c" ]
                        , PP.text "d"
                        ]
                    , PP.text "e"
                    ]
                    |> expectPP "[ a\n, [ b\n  , [ c ]\n  , d\n  ]\n, e\n]"
        , test "code wrap blow up 1" <|
            \_ ->
                args
                    [ PP.text "Level0"
                    , brackets
                        [ PP.text "Level1"
                        , apply
                            [ PP.text "Level2"
                            , brackets
                                [ PP.text "Level 3"
                                , args
                                    [ PP.text "Level4"
                                    , brackets
                                        [ PP.text "Level5"
                                        , apply
                                            [ PP.text "Level6"
                                            , brackets
                                                [ args
                                                    [ PP.text "Level7"
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                    |> expectPrettyPrint 1
                        (String.join "\n"
                            [ "Level0"
                            , "  [ Level1"
                            , "  , (Level2"
                            , "      [ Level 3"
                            , "      , Level4"
                            , "          [ Level5"
                            , "          , (Level6"
                            , "              [ Level7"
                            , "              ]"
                            , "            )"
                            , "          ]"
                            , "      ]"
                            , "    )"
                            , "  ]"
                            ]
                        )
        , test "code wrap blow up 2" <|
            \_ ->
                args
                    [ PP.text "Level0"
                    , brackets
                        [ PP.text "Level1"
                        , apply
                            [ PP.text "Level2"
                            , brackets
                                [ PP.text "Level 3"
                                , args
                                    [ PP.text "Level4"
                                    , brackets
                                        [ PP.text "Level5"
                                        , apply
                                            [ PP.text "Level6"
                                            , brackets
                                                [ args
                                                    [ PP.text "Level7"
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                    |> expectPrettyPrint 26
                        (String.join "\n"
                            [ "Level0"
                            , "  [ Level1"
                            , "  , (Level2"
                            , "      [ Level 3"
                            , "      , Level4"
                            , "          [ Level5"
                            , "          , (Level6"
                            , "              [ Level7 ]"
                            , "            )"
                            , "          ]"
                            , "      ]"
                            , "    )"
                            , "  ]"
                            ]
                        )
        ]


args : List Element -> Element
args entries =
    case entries of
        fst :: snd :: tail ->
            argsHelp tail [ PP.span [ fst, containerNewlineOrSpace, snd ] ]
                |> PP.span
                |> PP.indent 2

        fst :: tail ->
            argsHelp tail [ fst ]
                |> PP.span
                |> PP.indent 2

        [] ->
            PP.span []


argsHelp : List Element -> List Element -> List Element
argsHelp entries acc =
    case entries of
        head :: tail ->
            argsHelp tail <|
                head
                    :: containerNewlineOrSpace
                    :: acc

        [] ->
            List.reverse acc


apply : List Element -> Element
apply entries =
    case entries of
        fst :: snd :: tail ->
            PP.span <|
                [ applyHelp tail [ PP.span [ PP.text "(", fst, containerNewlineOrSpace, snd ] ]
                    |> PP.span
                    |> PP.indent 2
                , containerNewlineOrNothing
                , PP.text ")"
                ]

        fst :: tail ->
            PP.span <|
                [ applyHelp tail [ fst, PP.text "(" ]
                    |> PP.span
                    |> PP.indent 2
                , containerNewlineOrNothing
                , PP.text ")"
                ]

        [] ->
            PP.text "()"


applyHelp : List Element -> List Element -> List Element
applyHelp entries acc =
    case entries of
        head :: tail ->
            applyHelp tail <|
                head
                    :: containerNewlineOrSpace
                    :: acc

        [] ->
            List.reverse acc


brackets : List Element -> Element
brackets entries =
    case entries of
        fst :: tail ->
            PP.span <| bracketsHelp tail [ fst, PP.whitespace, PP.text "[" ]

        [] ->
            PP.text "[]"


bracketsHelp : List Element -> List Element -> List Element
bracketsHelp entries acc =
    case entries of
        head :: [] ->
            List.reverse <|
                PP.text "]"
                    :: containerNewlineOrSpace
                    :: (PP.indent 2 <| PP.span [ head ])
                    :: PP.text ", "
                    :: containerNewlineOrNothing
                    :: acc

        head :: tail ->
            bracketsHelp tail <|
                (PP.indent 2 <| PP.span [ head ])
                    :: PP.text ", "
                    :: containerNewlineOrNothing
                    :: acc

        [] ->
            List.reverse (PP.text "]" :: containerNewlineOrSpace :: acc)


containerNewlineOrSpace : Element
containerNewlineOrSpace =
    PP.break <| PP.onSpan <| PP.or PP.whitespace


containerNewlineOrNothing : Element
containerNewlineOrNothing =
    PP.break <| PP.onSpan <| PP.orNoop


expectPP : String -> Element -> Expect.Expectation
expectPP =
    expectPrettyPrint 12


expectPrettyPrint : Int -> String -> Element -> Expect.Expectation
expectPrettyPrint width expect content =
    Expect.equal expect (PP.stringify width content)
