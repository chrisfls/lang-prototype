module AST exposing (..)

import AST.Spec as Spec exposing (Spec)


type alias AST =
    -- TODO: add Doc
    { name : Maybe String
    , params : List Param
    , expose : Expose
    , imports : List Import
    , body : Body
    }


type alias Param =
    { name : String
    , spec : Spec
    }


type Expose
    = ExposeAll
    | ExposeSome (List Member)
    | ExposeSpec Spec


type Member
    = Value String
    | Spec String Bool


type alias Import =
    { name : String
    , from : String
    }


type Body
    = Body



-- format : AST -> Format.Text
-- format ast =
--     Format.rows False
--         [ formatModule ast.name
--         , formatParams ast.params
--         , formatExpose ast.expose
--         , Format.text ""
--         , formatImports ast.imports
--         , Format.text ""
--         ]
-- formatModule : Maybe String -> Format.Text
-- formatModule maybe =
--     case maybe of
--         Just name ->
--             Format.text <| "module " ++ name
--         Nothing ->
--             Format.text "module"
-- formatParams : List Param -> Format.Text
-- formatParams params =
--     Format.rows True <| List.map formatParam params
-- formatParam : Param -> Format.Text
-- formatParam param =
--     -- "  (" ++ param.name ++ " : " ++ Spec.format (width - 2) 1 param.spec ++ ")"
--     Debug.todo ""
-- formatExpose : Expose -> Format.Text
-- formatExpose expose =
--     case expose of
--         ExposeAll ->
--             Format.text "(..)"
--         ExposeSome members ->
--             -- Format.rows True
--             --     [ Format.text "expose"
--             --     ,
--             --     ]
--             --     ++ (List.map formatMember members
--             --             |> formatMembers
--             --             |> Format.format 1
--             --        )
--             Debug.todo ""
--         ExposeSpec spec ->
--             -- "expose\n  " ++ Spec.format 1 spec
--             Debug.todo ""
-- formatMember : Member -> Format.Text
-- formatMember member =
--     case member of
--         Value name ->
--             Format.text name
--         Spec name True ->
--             Format.text (name ++ "(..)")
--         Spec name False ->
--             Format.text name
-- formatMembers : List Format.Text -> Format.Text
-- formatMembers =
--     Format.span
--         { wrap = Just { start = "(", end = ")" }
--         , separator = Just ","
--         }
-- formatImports : List Import -> Format.Text
-- formatImports imports =
--     -- List.map formatImport imports
--     --     |> String.join "\n"
--     Debug.todo ""
-- formatImport : Import -> String
-- formatImport { name, from } =
--     "import " ++ name ++ " from \"" ++ from ++ "\""
