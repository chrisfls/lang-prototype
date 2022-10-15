module Codegen.ShittyJS exposing (generate)

import IR.Expr as Expr exposing (Expr)
import IR.Module as Module exposing (Module, ModuleBody)


runtime : String
runtime =
    "function $DebugPrint(msg){console.log(msg);return msg};function $DeclareModuleFactory(f){const cache=new Map();return (key)=>{const got=cache.get(key); if(got!==undefined)return got;const made=f(key);cache.set(key, made);return made;}}"


generate : Module -> String
generate modul =
    runtime ++ ";export default (" ++ generateModule modul ++ ")"


generateModule : Module -> String
generateModule modul =
    case modul of
        Module.Variable name ->
            name

        Module.Lambda name subExpr ->
            "$DeclareModuleFactory((" ++ name ++ ")=>(" ++ generateModule subExpr ++ "))"

        Module.Apply function argument ->
            let
                functionStr =
                    generateModule function

                argumentStr =
                    generateModule argument
            in
            "(" ++ functionStr ++ ")" ++ "(" ++ argumentStr ++ ")"

        Module.Annotation _ subExpr ->
            generateModule subExpr

        Module.IfEquals _ _ _ _ ->
            Debug.todo "Generate IfEquals"

        Module.Module body ->
            "(() => {const $exports={};" ++ generateModuleBody body ++ "})()"


generateModuleBody : ModuleBody -> String
generateModuleBody body =
    case body of
        Module.CloseModule ->
            ";$exports.main?.();return $exports"

        Module.DefSpec _ _ _ nextBody ->
            generateModuleBody nextBody

        Module.DefExpr privacy name expr nextBody ->
            let
                expose =
                    case privacy of
                        Module.Public ->
                            "=$exports." ++ name ++ "="

                        Module.Private ->
                            "="
            in
            "const " ++ name ++ expose ++ generateExpr expr ++ ";" ++ generateModuleBody nextBody


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
