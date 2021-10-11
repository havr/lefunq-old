open Common
open Ast_test__helpers
open Ast.Node
let test_tuple = test (Ast.Parser.expr()) Expr.pretty_print

let tests = [
  "test_empty", `Quick, (fun () -> test_tuple
    ~input: "()"
    ~expect: (Expr.Value (Value.Unit (Span.empty ())))
  );

  "test_empty_linebreak", `Quick, (fun () -> test_tuple
    ~input: "(

    )"
    ~expect: (Expr.Value (Value.Unit (Span.empty ())))
  );

  "test_multiple", `Quick, (fun () -> test_tuple
    ~input: "(1, 2)"
    ~expect: (Expr.Value (Value.Tuple Tuple.{range = Span.empty_range; exprs = [
      Expr.Value (Value.Int (Span.empty "1"));
      Expr.Value (Value.Int (Span.empty "2"));
    ]})));

  "test_multiple_linebreak", `Quick, (fun () -> test_tuple
    ~input: "(1,
    2)"
    ~expect: (Expr.Value (Value.Tuple Tuple.{range = Span.empty_range; exprs = [
      Expr.Value (Value.Int (Span.empty "1"));
      Expr.Value (Value.Int (Span.empty "2"));
    ]})))
]
