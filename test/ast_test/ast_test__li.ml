open Common
open Ast_test__helpers
open Ast.Node
let test_li = test (Ast.Parser.list()) Li.pretty_print

let single_int_value i = Li.Single (Expr.Value (Value.Int (Span.empty i)))
let single_spread_ident i = Li.Spread (Expr.Value (Value.Ident (Span.empty i)))

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
        single_int_value "1";
        single_int_value "2";
        single_int_value "3"
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
          single_int_value "1";
          single_int_value "2";
          single_int_value "3"
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
          single_int_value "1";
          single_int_value "2"
        ]
      }
  );

  "test_spread", `Quick, (fun () -> test_li
      ~input: "[
        ..a
      ]"
      ~expect: Li.{
        range = Span.empty_range;
        items = [
          single_spread_ident "a";
        ]
      }
  );   

  "test_spread_seq", `Quick, (fun () -> test_li
      ~input: "[
        ..a ..b; ..c 
      ]"
      ~expect: Li.{
        range = Span.empty_range;
        items = [
          single_spread_ident "a";
          single_spread_ident "b";
          single_spread_ident "c";
        ]
      }
  );   

"test_spread_value_mix", `Quick, (fun () -> test_li
    ~input: "[
      ..a; 1 ..b; 2; ..c 
    ]"
    ~expect: Li.{
      range = Span.empty_range;
      items = [
        single_spread_ident "a";
        single_int_value "1";
        single_spread_ident "b";
        single_int_value "2";
        single_spread_ident "c";
      ]
    }
);   

"test_spread_value_mix_newline", `Quick, (fun () -> test_li
    ~input: "[
      ..a;
      1 
      ..b
      2
      ..c 
    ]"
    ~expect: Li.{
      range = Span.empty_range;
      items = [
        single_spread_ident "a";
        single_int_value "1";
        single_spread_ident "b";
        single_int_value "2";
        single_spread_ident "c";
      ]
    }
);   
]