open Common
open Ast_test__helpers
open Ast.Node
let struct_def_test_case = test (Ast.Parser.expr()) Expr.pretty_print

let struct_def = [
    "tuple", `Quick, (fun () -> struct_def_test_case
        ~input: "(1, 2) ? { 
            | t -> ()
        }"
        ~expect: (Expr.Match (Match.{
            range = Span.empty_range;
            expr = Expr.Value(Value.Tuple(Tuple.{
                range = Span.empty_range;
                exprs = [
                    Expr.Value (Value.Int (Span.empty "1"));
                    Expr.Value (Value.Int (Span.empty "2"));
                ]
            }));
            block = Match.{
                range = Span.empty_range;
                cases = [
                    {
                        pattern = Param (Span.empty "t");
                        stmts = [Block.Expr (Expr.Value (Value.Unit (Span.empty ())))]
                    }
                ];
            }
        }))
    );
    "match", `Quick, (fun () -> struct_def_test_case
        ~input: "func hello ? { 
            | t -> 1
        }"
        ~expect: (Expr.Match (Match.{
            range = Span.empty_range;
            expr = Expr.Apply (Apply.{
                range = Span.empty_range;
                fn = Expr.Value (Value.Ident (Span.empty "func"));
                args = [
                    Apply.PosArg{expr = Expr.Value (Value.Ident(Span.empty "hello"))};
                ]
            });
            block = Match.{
                range = Span.empty_range;
                cases = [
                    {
                        pattern = Param (Span.empty "t");
                        stmts = [Block.Expr (Expr.Value (Value.Int (Span.empty "1")))]
                    }
                ];
            }
        }))
    )
]

let tests = struct_def @ [];