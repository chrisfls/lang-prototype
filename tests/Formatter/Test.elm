module Formatter.Test exposing (suite)

import Expect
import Format2 exposing (..)
import Test exposing (..)


brackets : List Text -> Text
brackets =
    span { wrap = Just { start = "[", end = "]" }, separator = Just "," }


apply : List Text -> Text
apply =
    span { wrap = Just { start = "(", end = ")" }, separator = Nothing }


args : List Text -> Text
args =
    span { wrap = Nothing, separator = Nothing }


suite : Test
suite =
    describe "Formatter"
        [ test "text" <|
            \_ ->
                expectFormat [ "text" ] <|
                    text "text"
        , test "empty span" <|
            \_ ->
                expectFormat [ "" ] <|
                    args []
        , test "1-element span" <|
            \_ ->
                expectFormat [ "a" ] <|
                    args [ text "a" ]
        , test "2-element span" <|
            \_ ->
                expectFormat [ "a b" ] <|
                    args [ text "a", text "b" ]
        , test "3-element span" <|
            \_ ->
                expectFormat [ "a b c" ] <|
                    args [ text "a", text "b", text "c" ]
        , test "1-depth span" <|
            \_ ->
                expectFormat
                    [ "Lor.em a"
                    , "  b"
                    , "  c"
                    , "  d"
                    ]
                <|
                    args
                        [ text "Lor.em"
                        , text "a"
                        , text "b"
                        , text "c"
                        , text "d"
                        ]
        , test "2-depth span" <|
            \_ ->
                expectFormat
                    [ "Lor.em a"
                    , "  b"
                    , "  yuck c"
                    , "    d"
                    , "    e"
                    , "    f"
                    , "  g"
                    ]
                <|
                    args
                        [ text "Lor.em"
                        , text "a"
                        , text "b"
                        , args
                            [ text "yuck"
                            , text "c"
                            , text "d"
                            , text "e"
                            , text "f"
                            ]
                        , text "g"
                        ]
        , test "3-depth span" <|
            \_ ->
                expectFormat
                    [ "Lor.em a"
                    , "  yuck b"
                    , "    puck c"
                    , "      d"
                    , "      e"
                    , "      f"
                    , "    g"
                    , "  h"
                    ]
                <|
                    args
                        [ text "Lor.em"
                        , text "a"
                        , args
                            [ text "yuck"
                            , text "b"
                            , args
                                [ text "puck"
                                , text "c"
                                , text "d"
                                , text "e"
                                , text "f"
                                ]
                            , text "g"
                            ]
                        , text "h"
                        ]
        , test "empty separatorless wrap" <|
            \_ ->
                expectFormat [ "()" ] <|
                    apply []
        , test "1-element separatorless wrap" <|
            \_ ->
                expectFormat [ "(a)" ] <|
                    apply [ text "a" ]
        , test "2-element separatorless wrap" <|
            \_ ->
                expectFormat [ "(a b)" ] <|
                    apply [ text "a", text "b" ]
        , test "3-element separatorless wrap" <|
            \_ ->
                expectFormat [ "(a b c)" ] <|
                    apply [ text "a", text "b", text "c" ]
        , test "1-depth separatorless wrap" <|
            \_ ->
                expectFormat
                    [ "(Lor.em a"
                    , "  b"
                    , "  c"
                    , "  d"
                    , ")"
                    ]
                <|
                    apply
                        [ text "Lor.em"
                        , text "a"
                        , text "b"
                        , text "c"
                        , text "d"
                        ]
        , test "2-depth separatorless wrap" <|
            \_ ->
                expectFormat
                    [ "(Lor.em a"
                    , "  b"
                    , "  (yuck c"
                    , "    d"
                    , "    e"
                    , "    f"
                    , "  )"
                    , "  g"
                    , ")"
                    ]
                <|
                    apply
                        [ text "Lor.em"
                        , text "a"
                        , text "b"
                        , apply
                            [ text "yuck"
                            , text "c"
                            , text "d"
                            , text "e"
                            , text "f"
                            ]
                        , text "g"
                        ]
        , test "3-depth separatorless wrap" <|
            \_ ->
                expectFormat
                    [ "(Lor.em a"
                    , "  (yuck b"
                    , "    (puck c"
                    , "      d"
                    , "      e"
                    , "      f"
                    , "    )"
                    , "    g"
                    , "  )"
                    , "  h"
                    , ")"
                    ]
                <|
                    apply
                        [ text "Lor.em"
                        , text "a"
                        , apply
                            [ text "yuck"
                            , text "b"
                            , apply
                                [ text "puck"
                                , text "c"
                                , text "d"
                                , text "e"
                                , text "f"
                                ]
                            , text "g"
                            ]
                        , text "h"
                        ]
        , test "empty wrap" <|
            \_ ->
                expectFormat [ "[]" ] <|
                    brackets []
        , test "1-element wrap" <|
            \_ ->
                expectFormat [ "[ a ]" ] <|
                    brackets [ text "a" ]
        , test "2-element wrap" <|
            \_ ->
                expectFormat [ "[ a, b ]" ] <|
                    brackets
                        [ text "a"
                        , text "b"
                        ]
        , test "32-element wrap" <|
            \_ ->
                expectFormat [ "[ a, b, c ]" ] <|
                    brackets
                        [ text "a"
                        , text "b"
                        , text "c"
                        ]
        , test "1-depth wrap" <|
            \_ ->
                expectFormat
                    [ "[ a"
                    , ", b"
                    , ", c"
                    , ", d"
                    , "]"
                    ]
                <|
                    brackets
                        [ text "a"
                        , text "b"
                        , text "c"
                        , text "d"
                        ]
        , test "2-depth wrap" <|
            \_ ->
                expectFormat
                    [ "[ a"
                    , ", [ b"
                    , "  , c"
                    , "  , d"
                    , "  ]"
                    , ", e"
                    , ", [ f ]"
                    , ", []"
                    , "]"
                    ]
                    (brackets <|
                        [ text "a"
                        , brackets
                            [ text "b"
                            , text "c"
                            , text "d"
                            ]
                        , text "e"
                        , brackets [ text "f" ]
                        , brackets []
                        ]
                    )
        , test "3-depth wrap" <|
            \_ ->
                expectFormat
                    [ "[ a"
                    , ", [ b"
                    , "  , [ c ]"
                    , "  , d"
                    , "  ]"
                    , ", e"
                    , "]"
                    ]
                    (brackets <|
                        [ text "a"
                        , brackets
                            [ text "b"
                            , brackets [ text "c" ]
                            , text "d"
                            ]
                        , text "e"
                        ]
                    )
        , test "code wrap blow up 1" <|
            \_ ->
                expectFormatWith 1
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
                <|
                    args
                        [ text "Level0"
                        , brackets
                            [ text "Level1"
                            , apply
                                [ text "Level2"
                                , brackets
                                    [ text "Level 3"
                                    , args
                                        [ text "Level4"
                                        , brackets
                                            [ text "Level5"
                                            , apply
                                                [ text "Level6"
                                                , brackets
                                                    [ args
                                                        [ text "Level7"
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
        , test "code wrap blow up 2" <|
            \_ ->
                expectFormatWith 26
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
                <|
                    args
                        [ text "Level0"
                        , brackets
                            [ text "Level1"
                            , apply
                                [ text "Level2"
                                , brackets
                                    [ text "Level 3"
                                    , args
                                        [ text "Level4"
                                        , brackets
                                            [ text "Level5"
                                            , apply
                                                [ text "Level6"
                                                , brackets
                                                    [ args
                                                        [ text "Level7"
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
        , test "1-depth rows" <|
            \_ ->
                expectFormatWith 1
                    [ "a"
                    , "b"
                    , "c"
                    , "d"
                    , "e"
                    ]
                <|
                    rows False
                        [ text "a"
                        , text "b"
                        , text "c"
                        , text "d"
                        , text "e"
                        ]
        , test "2-depth rows" <|
            \_ ->
                expectFormatWith 1
                    [ "a"
                    , "b"
                    , "c"
                    , "d"
                    , "e"
                    ]
                <|
                    rows False
                        [ text "a"
                        , rows False
                            [ text "b"
                            , text "c"
                            , text "d"
                            ]
                        , text "e"
                        ]
        , test "3-depth rows" <|
            \_ ->
                expectFormatWith 1
                    [ "a"
                    , "b"
                    , "c"
                    , "d"
                    , "e"
                    ]
                <|
                    rows False
                        [ text "a"
                        , rows False
                            [ text "b"
                            , rows False [ text "c" ]
                            , text "d"
                            ]
                        , text "e"
                        ]
        , test "1-depth indent rows" <|
            \_ ->
                expectFormatWith 1
                    [ "a"
                    , "b"
                    , "c"
                    , "d"
                    , "e"
                    ]
                <|
                    rows True
                        [ text "a"
                        , text "b"
                        , text "c"
                        , text "d"
                        , text "e"
                        ]
        , test "2-depth indent rows" <|
            \_ ->
                expectFormatWith 1
                    [ "a"
                    , "  b"
                    , "  c"
                    , "  d"
                    , "e"
                    ]
                <|
                    rows True
                        [ text "a"
                        , rows True
                            [ text "b"
                            , text "c"
                            , text "d"
                            ]
                        , text "e"
                        ]
        , test "3-depth indent rows" <|
            \_ ->
                expectFormatWith 1
                    [ "a"
                    , "  b"
                    , "    c"
                    , "  d"
                    , "e"
                    ]
                <|
                    rows True
                        [ text "a"
                        , rows True
                            [ text "b"
                            , rows True [ text "c" ]
                            , text "d"
                            ]
                        , text "e"
                        ]
        ]


expectFormat : List String -> Text -> Expect.Expectation
expectFormat =
    expectFormatWith 12


expectFormatWith : Int -> List String -> Text -> Expect.Expectation
expectFormatWith column expect entries =
    Expect.equal expect (String.split "\n" <| format column entries)
