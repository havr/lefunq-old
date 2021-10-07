open Ast_test__helpers

open Common
open Ast.Node

let test_case = test (Ast.Parser.func_decl ()) Func.pp

let tests = [
    "unit", `Quick, (fun () -> test_case 
        ~input: "test | () = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Positional (FuncParam.Unit)
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "unit", `Quick, (fun () -> test_case 
        ~input: "test | param: () = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Positional (FuncParam.Name FuncParam.{
                    name = Span.empty "param";
                    typ = Type.Unit
                })
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    )

    (* "test_no_args", `Quick, (fun () -> test_case
        ~input: "test | () = ()"
        ~expect: (Module.Module (Module.{
            range = Span.empty_range;
            keyword = Span.empty ();
            name = Span.empty "Name";
            entries = []
        }))
    );

    "test_nested", `Quick, (fun () -> test_case
        ~input: "Name = namespace {
            Inner = namespace {}
        }"
        ~expect: (Module.Module (Module.{
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
        }))
    );

    "test_with_entries", `Quick, (fun () -> test_case
        ~input: "Name = namespace {
            let a = 10
        }"
        ~expect: (Module.Module (Module.{
            range = Span.empty_range;
            keyword = Span.empty ();
            name = Span.empty "Name";
            entries = [
                Module.Let (Let.{
                    sig' = None;
                    range = Span.empty_range;
                    params = None;
                    ident = Span.empty "a";
                    expr = Let.Expr (Expr.Value (Value.Int (Span.empty "10")));
                    is_rec = false
                })
            ]
        }))
    ); *)
]