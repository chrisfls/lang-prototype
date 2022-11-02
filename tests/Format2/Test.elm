module Format2.Test exposing (suite)

import Expect
import Format2 as Format exposing (Content)
import Test exposing (..)


suite : Test
suite =
    describe "Format"
        [ primitives
        , extensions
        ]


primitives : Test
primitives =
    describe "primitives"
        [ test "text" <|
            \_ ->
                Format.text "a"
                    |> expectFormat "a"
        , test "empty span" <|
            \_ ->
                Format.span 0 []
                    |> expectFormat ""
        , test "1-elem span" <|
            \_ ->
                Format.span 0 [ Format.text "a" ]
                    |> expectFormat "a"
        , test "2-elem span" <|
            \_ ->
                Format.span 0 [ Format.text "a", Format.text "b" ]
                    |> expectFormat "ab"
        , test "3-elem span" <|
            \_ ->
                Format.span 0 [ Format.text "a", Format.text "b", Format.text "c" ]
                    |> expectFormat "abc"
        , test "1-depth span" <|
            \_ ->
                Format.span 0 [ Format.text "a" ]
                    |> expectFormat "a"
        , test "2-depth span" <|
            \_ ->
                Format.span 0 [ Format.text "a", Format.span 0 [ Format.text "b" ] ]
                    |> expectFormat "ab"
        , test "3-depth span" <|
            \_ ->
                Format.span 0 [ Format.text "a", Format.span 0 [ Format.text "b", Format.span 0 [ Format.text "c" ] ] ]
                    |> expectFormat "abc"
        , test "1-depth span indent" <|
            \_ ->
                Format.span 2
                    [ Format.breakpoint Format.force
                    , Format.text "a"
                    ]
                    |> expectFormat "\n  a"
        , test "2-depth span indent" <|
            \_ ->
                Format.span 2
                    [ Format.breakpoint Format.force
                    , Format.text "a"
                    , Format.span 2
                        [ Format.breakpoint Format.force
                        , Format.text "b"
                        , Format.breakpoint Format.force
                        , Format.text "c"
                        ]
                    , Format.breakpoint Format.force
                    , Format.text "d"
                    ]
                    |> expectFormat "\n  a\n    b\n    c\n  d"
        , test "3-depth span indent" <|
            \_ ->
                Format.span 2
                    [ Format.breakpoint Format.force
                    , Format.text "a"
                    , Format.span 2
                        [ Format.breakpoint Format.force
                        , Format.text "b"
                        , Format.span 2
                            [ Format.breakpoint Format.force
                            , Format.text "c"
                            ]
                        , Format.breakpoint Format.force
                        , Format.text "d"
                        ]
                    , Format.breakpoint Format.force
                    , Format.text "e"
                    ]
                    |> expectFormat "\n  a\n    b\n      c\n    d\n  e"
        , test "forced breakpoint" <|
            \_ ->
                Format.span 0 [ Format.text "a", Format.breakpoint Format.force, Format.text "b" ]
                    |> expectFormat "a\nb"
        , test "forced breakpoint indent" <|
            \_ ->
                Format.span 2 [ Format.text "a", Format.breakpoint Format.force, Format.text "b" ]
                    |> expectFormat "a\n  b"
        , test "useless optional breakpoint" <|
            \_ ->
                Format.span 0 [ Format.text "a", Format.breakpoint (Format.optional Nothing), Format.text "b" ]
                    |> expectFormat "ab"
        , test "useless continuation breakpoint" <|
            \_ ->
                Format.span 0 [ Format.text "a", Format.breakpoint (Format.container Nothing), Format.text "b" ]
                    |> expectFormat "ab"
        , test "useful optional breakpoint" <|
            \_ ->
                Format.span 0
                    [ Format.text "1abcdef"
                    , Format.breakpoint (Format.optional Nothing)
                    , Format.text "2abcdef"
                    , Format.breakpoint (Format.optional Nothing)
                    , Format.text "3abcdef"
                    , Format.breakpoint (Format.optional Nothing)
                    , Format.text "4abcdef"
                    ]
                    |> expectFormat "1abcdef\n2abcdef\n3abcdef\n4abcdef"
        , test "useful continuation breakpoint" <|
            \_ ->
                Format.span 0
                    [ Format.text "1abc"
                    , Format.breakpoint Format.force
                    , Format.text "2abc"
                    , Format.breakpoint (Format.container Nothing)
                    , Format.text "3abc"
                    , Format.breakpoint (Format.container Nothing)
                    , Format.text "4abc"
                    ]
                    |> expectFormat "1abc\n2abc\n3abc\n4abc"
        ]


extensions : Test
extensions =
    describe "extensions"
        [ test "1-element args" <|
            \_ ->
                args [ Format.text "a" ]
                    |> expectFormat "a"
        , test "2-element args" <|
            \_ ->
                args [ Format.text "a", Format.text "b" ]
                    |> expectFormat "a b"
        , test "3-element args" <|
            \_ ->
                args [ Format.text "a", Format.text "b", Format.text "c" ]
                    |> expectFormat "a b c"
        , test "1-depth args" <|
            \_ ->
                args
                    [ Format.text "Lorem.ip"
                    , Format.text "a"
                    , Format.text "b"
                    , Format.text "c"
                    ]
                    |> expectFormat
                        "Lorem.ip a\n  b\n  c"
        , test "2-depth args" <|
            \_ ->
                args
                    [ Format.text "Lor.em"
                    , Format.text "a"
                    , Format.text "b"
                    , args
                        [ Format.text "yuck"
                        , Format.text "c"
                        , Format.text "d"
                        , Format.text "e"
                        , Format.text "f"
                        ]
                    , Format.text "g"
                    ]
                    |> expectFormat "Lor.em a\n  b\n  yuck c\n    d\n    e\n    f\n  g"
        , test "3-depth args" <|
            \_ ->
                args
                    [ Format.text "Lor.em"
                    , Format.text "a"
                    , args
                        [ Format.text "yuck"
                        , Format.text "b"
                        , args
                            [ Format.text "puck"
                            , Format.text "c"
                            , Format.text "d"
                            , Format.text "e"
                            , Format.text "f"
                            ]
                        , Format.text "g"
                        ]
                    , Format.text "h"
                    ]
                    |> expectFormat
                        "Lor.em a\n  yuck b\n    puck c\n      d\n      e\n      f\n    g\n  h"
        , test "empty apply" <|
            \_ ->
                apply []
                    |> expectFormat "()"
        , test "1-element apply" <|
            \_ ->
                apply [ Format.text "a" ]
                    |> expectFormat "(a)"
        , test "2-element apply" <|
            \_ ->
                apply [ Format.text "a", Format.text "b" ]
                    |> expectFormat "(a b)"
        , test "3-element apply" <|
            \_ ->
                apply [ Format.text "a", Format.text "b", Format.text "c" ]
                    |> expectFormat "(a b c)"
        , test "1-depth apply" <|
            \_ ->
                apply
                    [ Format.text "Lor.em"
                    , Format.text "a"
                    , Format.text "b"
                    , Format.text "c"
                    , Format.text "d"
                    ]
                    |> expectFormat "(Lor.em a\n  b\n  c\n  d\n)"
        , test "2-depth apply" <|
            \_ ->
                apply
                    [ Format.text "Lor.em"
                    , Format.text "a"
                    , Format.text "b"
                    , apply
                        [ Format.text "yuck"
                        , Format.text "c"
                        , Format.text "d"
                        , Format.text "e"
                        , Format.text "f"
                        ]
                    , Format.text "g"
                    ]
                    |> expectFormat "(Lor.em a\n  b\n  (yuck c\n    d\n    e\n    f\n  )\n  g\n)"
        , test "3-depth apply" <|
            \_ ->
                apply
                    [ Format.text "Lor.em"
                    , Format.text "a"
                    , apply
                        [ Format.text "yuck"
                        , Format.text "b"
                        , apply
                            [ Format.text "puck"
                            , Format.text "c"
                            , Format.text "d"
                            , Format.text "e"
                            , Format.text "f"
                            ]
                        , Format.text "g"
                        ]
                    , Format.text "h"
                    ]
                    |> expectFormat "(Lor.em a\n  (yuck b\n    (puck c\n      d\n      e\n      f\n    )\n    g\n  )\n  h\n)"
        , test "empty brackets" <|
            \_ ->
                brackets []
                    |> expectFormat "[]"
        , test "1-element brackets" <|
            \_ ->
                brackets [ Format.text "a" ]
                    |> expectFormat "[ a ]"
        , test "2-element brackets" <|
            \_ ->
                brackets
                    [ Format.text "a"
                    , Format.text "b"
                    ]
                    |> expectFormat "[ a, b ]"
        , test "32-element brackets" <|
            \_ ->
                brackets
                    [ Format.text "a"
                    , Format.text "b"
                    , Format.text "c"
                    ]
                    |> expectFormat "[ a, b, c ]"
        , test "1-depth brackets" <|
            \_ ->
                brackets
                    [ Format.text "a"
                    , Format.text "b"
                    , Format.text "c"
                    , Format.text "d"
                    ]
                    |> expectFormat "[ a\n, b\n, c\n, d\n]"
        , test "2-depth brackets" <|
            \_ ->
                brackets
                    [ Format.text "a"
                    , brackets
                        [ Format.text "b"
                        , Format.text "c"
                        , Format.text "d"
                        ]
                    , Format.text "e"
                    , brackets [ Format.text "f" ]
                    , brackets []
                    ]
                    |> expectFormat "[ a\n, [ b\n  , c\n  , d\n  ]\n, e\n, [ f ]\n, []\n]"
        , test "3-depth brackets" <|
            \_ ->
                brackets
                    [ Format.text "a"
                    , brackets
                        [ Format.text "b"
                        , brackets [ Format.text "c" ]
                        , Format.text "d"
                        ]
                    , Format.text "e"
                    ]
                    |> expectFormat "[ a\n, [ b\n  , [ c ]\n  , d\n  ]\n, e\n]"
        , test "code wrap blow up 1" <|
            \_ ->
                args
                    [ Format.text "Level0"
                    , brackets
                        [ Format.text "Level1"
                        , apply
                            [ Format.text "Level2"
                            , brackets
                                [ Format.text "Level 3"
                                , args
                                    [ Format.text "Level4"
                                    , brackets
                                        [ Format.text "Level5"
                                        , apply
                                            [ Format.text "Level6"
                                            , brackets
                                                [ args
                                                    [ Format.text "Level7"
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                    |> expectFormatWith 1
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
                    [ Format.text "Level0"
                    , brackets
                        [ Format.text "Level1"
                        , apply
                            [ Format.text "Level2"
                            , brackets
                                [ Format.text "Level 3"
                                , args
                                    [ Format.text "Level4"
                                    , brackets
                                        [ Format.text "Level5"
                                        , apply
                                            [ Format.text "Level6"
                                            , brackets
                                                [ args
                                                    [ Format.text "Level7"
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                    |> expectFormatWith 26
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


args : List Content -> Content
args entries =
    case entries of
        fst :: snd :: tail ->
            Format.span 2 <|
                argsHelp tail [ Format.span 0 [ fst, containerNewlineOrSpace, snd ] ]

        fst :: tail ->
            Format.span 2 <| argsHelp tail [ fst ]

        [] ->
            Format.span 0 []


argsHelp : List Content -> List Content -> List Content
argsHelp entries acc =
    case entries of
        head :: tail ->
            argsHelp tail <|
                head
                    :: containerNewlineOrSpace
                    :: acc

        [] ->
            List.reverse acc


apply : List Content -> Content
apply entries =
    case entries of
        fst :: snd :: tail ->
            Format.span 0 <|
                [ Format.span 2 <|
                    applyHelp tail [ Format.span 0 [ Format.text "(", fst, containerNewlineOrSpace, snd ] ]
                , containerNewlineOrNothing
                , Format.text ")"
                ]

        fst :: tail ->
            Format.span 0 <|
                [ Format.span 2 <| applyHelp tail [ fst, Format.text "(" ]
                , containerNewlineOrNothing
                , Format.text ")"
                ]

        [] ->
            Format.text "()"


applyHelp : List Content -> List Content -> List Content
applyHelp entries acc =
    case entries of
        head :: tail ->
            applyHelp tail <|
                head
                    :: containerNewlineOrSpace
                    :: acc

        [] ->
            List.reverse acc


brackets : List Content -> Content
brackets entries =
    case entries of
        fst :: tail ->
            Format.span 0 <| bracketsHelp tail [ fst, space, Format.text "[" ]

        [] ->
            Format.text "[]"


bracketsHelp : List Content -> List Content -> List Content
bracketsHelp entries acc =
    case entries of
        head :: [] ->
            List.reverse <| Format.text "]" :: containerNewlineOrSpace :: Format.span 2 [ head ] :: Format.text ", " :: containerNewlineOrNothing :: acc

        head :: tail ->
            bracketsHelp tail <|
                Format.span 2 [ head ]
                    :: Format.text ", "
                    :: containerNewlineOrNothing
                    :: acc

        [] ->
            List.reverse (Format.text "]" :: containerNewlineOrSpace :: acc)


containerNewlineOrSpace : Content
containerNewlineOrSpace =
    Format.breakpoint (Format.container (Just " "))


containerNewlineOrNothing : Content
containerNewlineOrNothing =
    Format.breakpoint (Format.container Nothing)


expectFormat : String -> Content -> Expect.Expectation
expectFormat =
    expectFormatWith 12


expectFormatWith : Int -> String -> Content -> Expect.Expectation
expectFormatWith width expect content =
    Expect.equal expect (Format.format width content)


space : Content
space =
    Format.text " "
