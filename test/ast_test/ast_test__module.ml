open Ast_test__helpers

open Common
open Ast.Node

let test_case = test (Ast.Parser.modu()) Module.pretty_print

let tests = [
    "test_empty", `Quick, (fun () -> test_case
        ~input: "module Name = {}"
        ~expect: Module.{
            range = Span.empty_range;
            keyword = Span.empty ();
            name = Span.empty "Name";
            entries = []
        }
    );

    "test_nested", `Quick, (fun () -> test_case
        ~input: "module Name = {
            module Inner = {}
        }"
        ~expect: Module.{
            range = Span.empty_range;
            keyword = Span.empty ();
            name = Span.empty "Name";
            entries = [
                Module.Module (Module.{
                    range = Span.empty_range;
                    keyword = Span.empty ();
                    name = Span.empty "Inner";
                    entries = []
                })
            ]
        }
    );

    "test_with_entries", `Quick, (fun () -> test_case
        ~input: "module Name = {
            let a = 10
        }"
        ~expect: Module.{
            range = Span.empty_range;
            keyword = Span.empty ();
            name = Span.empty "Name";
            entries = [
                Module.Let (Let.{
                    sig' = None;
                    range = Span.empty_range;
                    args = None;
                    ident = Span.empty "a";
                    expr = Let.Expr (Expr.Value (Value.Int (Span.empty "10")));
                    is_rec = false
                })
            ]
        }
    );
]