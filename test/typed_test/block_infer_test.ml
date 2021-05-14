(* TODO: wtf?? *)

(* open Common
open Typed
open Base

let todo_hello () =
    let input = {|
        let b = \a {a}
        b
    |} in
    match Ast.parse_custom ~file_name: "" (Ast.Comb.((Ast.Parser.block_stmts ()).fn)) input with
    | Error _ -> Alcotest.fail ""; (* TODO: proper error string *)
    | Ok stmts -> begin 
        let block = Ast.Node.Block.{
            stmts = stmts;
            range = Span.empty_range;
        } in
        let transformed = Typed.Transform.block block in
        let resolve_ctx = Resolve.Ctx.make ~resolve_source: (fun _ -> (raise Common.Unreachable)) (Resolver.Scope.toplevel (Resolver.Scope.root "")) in
        let block = Resolve.block resolve_ctx transformed in
        let infer_ctx = Typed.Inferno.make_ctx ~debug: false ~env: (Inferno.make_env()) in
        let typed = Infer.block ~ctx: infer_ctx block in
        Alcotest.(check bool) "result types are equal" true @@ Typed.Type.equals typed (Typed.Type.lambda [Typed.Type.Var "p0"; Typed.Type.Var "p0"]).typ;
    end


let tests = [
    "simple_lambda", `Quick, todo_hello
] *)