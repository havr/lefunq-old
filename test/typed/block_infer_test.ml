open Parlex
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
            start_pos = Pos.empty;
            end_pos = Pos.empty;
        } in
        let transformed = Typed.Transform.block block in
        let resolve_ctx = Resolve.make_context (Resolver.Local.make (Resolver.Global.root())) in
        let block = Resolve.block resolve_ctx transformed in
        let infer_ctx = Infer.make_ctx () in
        let env = Map.empty(module String) in
        let substs = Map.empty(module String) in
        let (_, typed) = Infer.block ~ctx: infer_ctx env substs block in
        Alcotest.(check bool) "result types are equal" true @@ Typed.Type.equals typed (Typed.Type.lambda [Typed.Type.Var "p0"; Typed.Type.Var "p0"]).typ;
    end


let tests = [
    "simple_lambda", `Quick, todo_hello
]