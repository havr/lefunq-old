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

let tests = [
    ("ast:ident", Ast_test__ident.tests);
    ("ast:tuple", Ast_test__tuple.tests);
    ("ast:module", Ast_test__module.tests);
    ("ast:match", Ast_test__match.tests);
    ("ast:named_params", Ast_test__named_params.tests);
    ("ast:list", Ast_test__li.tests);
    ("ast:typedef", Ast_test__typedef.tests);
    ("ast:namespace", Ast_test__namespace.tests);
    ("ast:func", Ast_test__func.tests);
    ("ast:destruct", Ast_test__destruct.tests);
  ]