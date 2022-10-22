module Codegen.ShittyJS exposing (generate)

import IR.Expr as Expr exposing (Expr)
import IR.Module as Module exposing (Module, ModuleBody, ModuleExpr)


runtime : String
runtime =
    "function $DebugPrint(msg){console.log(msg);return msg};function $DeclareModuleFactory(f){const cache=new Map();return (key)=>{const got=cache.get(key); if(got!==undefined)return got;const made=f(key);cache.set(key, made);return made;}}"


generate : Module -> String
generate modul =
    runtime ++ ";export default (" ++ generateModule modul ++ ")"


generateModule : Module -> String
generateModule modul =
    case modul of
        Module.Param name _ subExpr ->
            "$DeclareModuleFactory((" ++ name ++ ")=>(" ++ generateModule subExpr ++ "))"

        Module.Let name subExpr nextModule ->
            "const " ++ name ++ "=" ++ generateModuleExpr subExpr ++ generateModule nextModule

        Module.ModuleBody body ->
            "(() => {const $exports={};" ++ generateModuleBody body ++ "})()"


generateModuleExpr : ModuleExpr -> String
generateModuleExpr expr =
    case expr of
        Module.Variable name ->
            name

        Module.Apply function argument ->
            let
                functionStr =
                    generateModuleExpr function

                argumentStr =
                    generateModuleExpr argument
            in
            "(" ++ functionStr ++ ")" ++ "(" ++ argumentStr ++ ")"

        Module.Import _ ->
            Debug.todo "Module.Import"


generateModuleBody : ModuleBody -> String
generateModuleBody body =
    case body of
        Module.ReturnModule ->
            ";$exports.main?.();return $exports"

        Module.DefSpec _ _ nextBody ->
            generateModuleBody nextBody

        Module.DefExpr name expr nextBody ->
            "const " ++ name ++ "=$exports." ++ name ++ "=" ++ generateExpr expr ++ ";" ++ generateModuleBody nextBody


generateExpr : Expr -> String
generateExpr expr =
    case expr of
        Expr.Variable name ->
            name

        Expr.Lambda _ name subExpr ->
            "(" ++ name ++ ")=>(" ++ generateExpr subExpr ++ ")"

        Expr.Apply function argument ->
            let
                functionStr =
                    generateExpr function

                argumentStr =
                    generateExpr argument
            in
            "(" ++ functionStr ++ ")" ++ "(" ++ argumentStr ++ ")"

        Expr.Annotation _ subExpr ->
            generateExpr subExpr
