open Common
open Ast_test__helpers
let test_ident = test Ast.Parser.Lexemes.ident Ast.Node.Ident.pretty_print

let test_simple () = test_ident
  ~input: "hello"
  ~expect: (Span.empty "hello")

let test_qualified () = test_ident
  ~input: "hello.world"
  ~expect: (Span.empty "hello.world")

let tests = [
  "simple", `Quick, test_simple;
  "qualified", `Quick, test_qualified
]
