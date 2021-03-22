module TestHelpers = struct 
  let a = Ast.Parser.block_stmts ()
  (* let print_sexp sexp =
    let buf = Buffer.create 0 in
    let fmt = Format.formatter_of_buffer buf in
    let open Core_kernel in
    Sexp.pp_hum fmt sexp;
    Format.pp_print_flush fmt ();
    Buffer.contents buf *)


  (* let parse parser ~printer ~lexemes ~expect = 
    let open Parlex in 
    let convert_ast_to_sexp ast = printer ast |> print_sexp in
    let input = lexemes 
        |> List.map (fun lexeme -> Parlex.{end_pos = Pos.empty; start_pos = Pos.empty; value = lexeme}) in
    let state = Ast.Comb.State.make input in
    let a = Ast.Comb.(parser.fn state) in
    match (a, expect) with
    | (Ok(v, _), Some(expect_ast)) -> 
      let expect_sexp = convert_ast_to_sexp expect_ast in
      let got_sexp = convert_ast_to_sexp v in
        Alcotest.(check string) "sexp compare" expect_sexp got_sexp
    | Error e, Some _ -> 
      Alcotest.fail ("unexpected " ^ (Pos.to_string e.position) ^ ":" ^ e.message)
    | Ok(v, _), None -> 
      let got_sexp = convert_ast_to_sexp v in
      Alcotest.fail ("unexpected match " ^ got_sexp)
    | Error _, None -> () *)
end

(*
module Helpers = struct 
  open Parlex
  open Ast.Node

  let str s = StrValue {str_pos = Pos.empty; str=s}
  let str_expr s = ValueExpr (StrValue {str_pos = Pos.empty; str=s})
  let int i = IntValue {int_pos = Pos.empty; int=i}
  let int_expr i = ValueExpr(IntValue {int_pos = Pos.empty; int=i})
  let ident id = IdentValue {ident_pos = Pos.empty; ident=id}
  let ident_expr id = ValueExpr(IdentValue {ident_pos = Pos.empty; ident=id})
  let apply fn args = ApplyExpr {apply_fn = fn; apply_args = args}
  let apply_ident id args = ApplyExpr {apply_fn = (ValueExpr (ident id)); apply_args = args}
end

module ExpressionTest = struct 
  module Lexeme = struct
    type t = string
  end

open Ast
open TestHelpers

let fn_value () = 
  let open Helpers in
  parse (Ast.Parser.fn) 
    ~lexemes:[Lexeme.Ident "hello"]
    ~printer: Ast.Print.convert_expr
    ~expect: (Some (ident_expr "hello"))

let fn_test () = 
  let open Helpers in
  parse (Ast.Parser.fn) 
    ~lexemes:[Lexeme.Ident "hello"; Lexeme.Ident "world"]
    ~printer: Ast.Print.convert_expr
    ~expect: (Some (apply (ident_expr "hello") [ident_expr "world"]))

let fn_chain () = 
  let open Helpers in
  parse (Ast.Parser.fn) 
    ~lexemes:[Lexeme.Ident "hello"; Lexeme.Ident "world"; Lexeme.Int "100"; Lexeme.Str "meow"]
    ~printer: Ast.Print.convert_expr
    ~expect: (Some (apply (ident_expr "hello") [ident_expr "world"; int_expr "100"; str_expr "meow"]))

let fn_sep () = 
  let open Helpers in
  let open Lexeme in
  parse (Ast.Parser.fn) 
    ~lexemes:[Ident "hello"; Ident "world"; Op "+"; Lexeme.Str "meow"]
    ~printer: Ast.Print.convert_expr
    ~expect: (Some (apply (ident_expr "hello") [ident_expr "world"]))

let unary_ok () = 
  let open Helpers in
  let open Lexeme in
  parse (Ast.Parser.unary) 
    ~lexemes:[Op "-"; Op "-"; Lexeme.Int "10"]
    ~printer: Ast.Print.convert_expr
    ~expect: (Some (apply (ident_expr "-") [apply (ident_expr "-") [int_expr "10"]]))

let binary_simple () = 
  let open Helpers in
  let open Lexeme in
  parse (Ast.Parser.binary) 
    ~lexemes:[Int "2"; Op "+"; Int "3"]
    ~printer: Ast.Print.convert_expr
    ~expect: (Some (apply (ident_expr "+") [int_expr "2"; int_expr "3"]))

let binary_precedence () = 
  let open Helpers in
  let open Lexeme in
  parse (Ast.Parser.binary) 
    ~lexemes:[Ident "println"; Op "$"; Int "2"; Op "+"; Int "3"; Op "*"; Int "4"]
    ~printer: Ast.Print.convert_expr
    ~expect: (Some (apply_ident "$" [ident_expr "println"; apply_ident "+" [int_expr "2"; apply_ident "*" [int_expr "3"; int_expr "4"]]]))

  let tests = [
    "fn_test", `Quick, fn_test;
    "fn_value", `Quick, fn_value;
    "fn_chain", `Quick, fn_chain;
    "fn_sep", `Quick, fn_sep;
    "unary:ok", `Quick, unary_ok;
    "binary_simple:ok", `Quick, binary_simple;
    "binary_precedence:ok", `Quick, binary_precedence;
  ]
end

let () = Alcotest.run "Ast" [
  "Expression", ExpressionTest.tests;
] *)

open Common
open Ast_test__helpers

module Tuple = struct
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
end

module Li = struct
  open Ast.Node
  let test_li = test (Ast.Parser.list()) Li.pretty_print

  let tests = [
    "test_empty", `Quick, (fun () -> test_li
      ~input: "[]"
      ~expect: Li.{
        range = Span.empty_range;
        items = []
      }
    );

    "test_empty_linebreak", `Quick, (fun() -> test_li
      ~input: "[
      ]"
      ~expect: Li.{
        range = Span.empty_range;
        items = []
      }
    );

    "test_values", `Quick, (fun () -> test_li
      ~input: "[1; 2; 3]"
      ~expect: Li.{
        range = Span.empty_range;
        items = [
          Expr.Value (Value.Int (Span.empty "1"));
          Expr.Value (Value.Int (Span.empty "2"));
          Expr.Value (Value.Int (Span.empty "3"));
        ]
      }
    );

    "test_values_newline", `Quick, (fun () -> test_li
        ~input: "[
          1
          2
          3
        ]"
        ~expect: Li.{
          range = Span.empty_range;
          items = [
            Expr.Value (Value.Int (Span.empty "1"));
            Expr.Value (Value.Int (Span.empty "2"));
            Expr.Value (Value.Int (Span.empty "3"));
          ]
        }
    );   

    "test_values_newline_semicolon", `Quick, (fun () -> test_li
        ~input: "[
          1;
          2
        ]"
        ~expect: Li.{
          range = Span.empty_range;
          items = [
            Expr.Value (Value.Int (Span.empty "1"));
            Expr.Value (Value.Int (Span.empty "2"));
          ]
        }
    )
  ]
end

module Ident = struct 
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

end
(* module Let = struct 
  let test_let = test (Ast.Parser.let_()) Ast.Node.Let.tree_repr

  let test_simple () = test_import 
    ~input: {|import "hello"|}
    ~expect: Ast.Node.Import.{
      keyword = Span.empty "import";
      source = Span.empty "hello";
      name = None;
    }

  let test_named () = test_import 
    ~input: {|import Hello "hello"|}
    ~expect: Ast.Node.Import.{
      keyword = Span.empty "import";
      source = Span.empty "hello";
      name = Some (Span.empty "Hello");
    }

  let tests = [
    "simple", `Quick, test_simple;
    "named", `Quick, test_named;
  ]
end *)

(* module Import = struct 
  let test_import = test Ast.Parser.import Ast.Node.Import.tree_repr

  let test_simple () = test_import 
    ~input: {|import "hello"|}
    ~expect: Ast.Node.Import.{
      keyword = Span.empty "import";
      source = Span.empty "hello";
      name = None;
    }

  let test_named () = test_import 
    ~input: {|import Hello "hello"|}
    ~expect: Ast.Node.Import.{
      keyword = Span.empty "import";
      source = Span.empty "hello";
      name = Some (Span.empty "Hello");
    }

  let tests = [
    "simple", `Quick, test_simple;
    "named", `Quick, test_named;
  ]
end *)
(* module Import = struct 
  open Common

  let test_import = test Ast.Parser.import Ast.Node.Import.tree_repr

  let test_simple () = test_import 
    ~input: "import Foo.Bar"
    ~expect: Ast.Node.Import.{
      keyword = Span.empty "import";
      name = Span.empty "Foo.Bar";
      kind = None;
    }

  let test_rename () = test_import 
    ~input: "import Foo.Bar as Quux"
    ~expect: Ast.Node.Import.{
      keyword = Span.empty "import";
      name = Span.empty "Foo.Bar";
      kind = Some (Rename (Span.empty "Quux"))
    }

  let test_nested () = test_import 
    ~input: {|import Foo.Bar (hello, world (foo, bar), rename as newname)|}
    ~expect: Ast.Node.Import.{
      keyword = Span.empty "import";
      name = Span.empty "Foo.Bar";
      kind = Some (Names [
        (Span.empty "hello", None);
        (Span.empty "world", Some (Names [
          (Span.empty "foo", None);
          (Span.empty "bar", None);
        ]));
        (Span.empty "rename", Some (Rename (Span.empty "newname")))
      ])
    }

  let test_newline_tolerance () = test_import 
    ~input: {|import Foo.Bar (
      hello
      world ( foo
      bar )
      rename as newname )|}
    ~expect: Ast.Node.Import.{
      keyword = Span.empty "import";
      name = Span.empty "Foo.Bar";
      kind = Some (Names [
        (Span.empty "hello", None);
        (Span.empty "world", Some (Names [
          (Span.empty "foo", None);
          (Span.empty "bar", None);
        ]));
        (Span.empty "rename", Some (Rename (Span.empty "newname")))
      ])
    }

  let test_newline_tolerance_2 () = test_import
    ~input: {|import Foo (
          bar
          baz as a, quux as b
          nested (hello, world as neko)
      )
    |}
    ~expect: Ast.Node.Import.{
      keyword = Span.empty "import";
      name = Span.empty "Foo";
      kind = Some (Names [
        (Span.empty "bar", None);
        (Span.empty "baz", Some (Rename (Span.empty "a")));
        (Span.empty "quux", Some (Rename (Span.empty "b")));
        (Span.empty "nested", Some (Names [
          (Span.empty "hello", None);
          (Span.empty "world", Some (Rename (Span.empty "neko")));
        ]));
      ])
    }

  let tests = [
    "simple", `Quick, test_simple;
    "rename", `Quick, test_rename;
    "nested", `Quick, test_nested;
    "newline_tolerance", `Quick, test_newline_tolerance;
    "newline_tolerance_2", `Quick, test_newline_tolerance_2
  ]
end *)

let tests = [
    ("Ast:ident", Ident.tests);
    ("Ast:tuple", Tuple.tests);
    ("Ast:module", Ast_test__module.tests);
    ("Ast:list", Li.tests)
  ]