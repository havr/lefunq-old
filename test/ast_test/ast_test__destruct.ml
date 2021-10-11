open Ast_test__helpers

open Common
open Ast.Node

let test_case = test (Ast.Parser.destruct_decl ()) Let.pretty_print

let tests = [
    "positional_unit", `Quick, (fun () -> test_case 
        ~input: "test = 10"
        ~expect: Let.{
            sig' = None;
            range = Span.empty_range;
            params = None;
            ident = Span.empty "test";
            expr = Let.Expr (Expr.Value (Value.Int (Span.empty "10")));
            is_rec = false ;
        }
    );
]