module Formatter.Test exposing (suite)

import Expect
import Formatter exposing (..)
import Test exposing (..)


brackets : List Entries -> Entries
brackets =
    wrap { start = "[", separator = Just ",", end = "]" }


apply : List Entries -> Entries
apply =
    wrap { start = "(", separator = Nothing, end = ")" }


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
                    span []
        , test "1-element span" <|
            \_ ->
                expectFormat [ "a" ] <|
                    span [ text "a" ]
        , test "2-element span" <|
            \_ ->
                expectFormat [ "a b" ] <|
                    span [ text "a", text "b" ]
        , test "3-element span" <|
            \_ ->
                expectFormat [ "a b c" ] <|
                    span [ text "a", text "b", text "c" ]
        , test "1-depth span" <|
            \_ ->
                expectFormat
                    [ "Lor.em a"
                    , "  b"
                    , "  c"
                    , "  d"
                    ]
                <|
                    span
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
                    span
                        [ text "Lor.em"
                        , text "a"
                        , text "b"
                        , span
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
                    span
                        [ text "Lor.em"
                        , text "a"
                        , span
                            [ text "yuck"
                            , text "b"
                            , span
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
        , test "word wrap blow up" <|
            \_ ->
                expectFormat
                    [ "Level0"
                    , "  [ Level1"
                    , "  , (Level2"
                    , "      [ Level3"
                    , "          [ Level4"
                    , "          , (Level5"
                    , "              [ Level6"
                    , "              ]"
                    , "            )"
                    , "          ]"
                    , "      ]"
                    , "    )"
                    , "  ]"
                    ]
                <|
                    span
                        [ text "Level0"
                        , brackets
                            [ text "Level1"
                            , apply
                                [ text "Level2"
                                , brackets
                                    [ span
                                        [ text "Level3"
                                        , brackets
                                            [ text "Level4"
                                            , apply
                                                [ text "Level5"
                                                , brackets
                                                    [ span
                                                        [ text "Level6"
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        ]
        ]


expectFormat : List String -> Entries -> Expect.Expectation
expectFormat =
    expectFormatWith 12


expectFormatWith : Int -> List String -> Entries -> Expect.Expectation
expectFormatWith column expect entries =
    Expect.equal expect (String.split "\n" <| format column entries)
