(* open Typed_test__infer__helper
open Helper

let resolved name = Typed.Symbol.Resolved.make name @@ Some (Typed.Symbol.Id.make "" [] name)
module Fake = struct 
  let local_ident name type_ = 
    Typed.Ident.{
      range = Common.Span.empty_range;
      resolved = resolved name;
      resolution = [];
      scheme = Some (Typed.Type.make_scheme [] type_); (* TODO: is it really needed here *)
    }

  let global_ident name type_ = 
    Typed.Ident.{
      range = Common.Span.empty_range;
      resolved = resolved name;
      resolution = [];
      scheme = Some (Typed.Type.make_scheme [] type_); (* TODO: is it really needed here *)
    }
end

let test_todo () = Alcotest.fail "todo"

open Base



(*
  let test ~src ~dst ?(errors=[]) ~expect () =
    let ctx = Typed.Infer.make_ctx () in
    let unified = Typed.Infer.unify ~ctx src dst in
    let results = results expect unified.unified
    in begin if List.length results > 0 then
        Alcotest.fail (String.concat ~sep: "\n" results) end;
    begin 
        let errors_expected_not_found = difference 
          ~equals: (Infer.error_equals) errors (Basket.get ctx.errors) in
        let unexpected_got = difference 
          ~equals: (Infer.error_equals) (Basket.get ctx.errors) errors in
        let map_errors errors = errors |> List.map ~f: (fun err ->
          match err with
          | Unify.Mismatch {expected; got} -> 
            (Type.to_string expected) ^ " != " ^ (Type.to_string got)
        ) in
        let exp_not_found = if List.length errors_expected_not_found > 0 then
          map_errors errors_expected_not_found 
          |> String.concat ~sep: "\n" else "" in
        let unexp = if List.length unexpected_got > 0 then
          map_errors unexpected_got 
          |> String.concat ~sep: "\n" else "" in
        let labeled_exp = if String.is_empty exp_not_found then
          "" else "expected: " ^ exp_not_found in
        let labeled_unexp = if String.is_empty unexp then
          "" else "unexpected: " ^ unexp in

        let issues = [labeled_exp; labeled_unexp] 
        |> List.filter ~f: (fun s -> not @@ String.is_empty s) in
        if List.length issues > 0 then
          issues 
            |> String.concat ~sep: "\n"
            |> Alcotest.fail
            |> ignore
      end

      

  let test_var_src_ok () = 
    test 
      ~src: (Type.Var "t0")
      ~dst: (Type.Simple "Int")
      ~expect: [
        "t0", Type.Simple "Int"
      ] ()

  let test_var_dst_ok () = 
    test 
      ~src: (Type.Simple "Int")
      ~dst: (Type.Var "t0")
      ~expect: [
        "t0", Type.Simple "Int"
      ] ()

  let test_simple_ok () = 
    test 
      ~src: (Type.Simple "Int")
      ~dst: (Type.Simple "Int")
      ~expect: [] ()

  let test_simple_mismatch () = 
    test 
      ~src: (Type.Simple "Int")
      (* ~dst: (Type.Simple "Str") *)
      ~errors: [
        Typed.Unify.Mismatch {
          expected = Type.Simple "Int";
          got = Type.Simple "Str"
        }
      ]
      ~expect: []
      ()
      
  let test_tuple_mismatch () = 
    test 
      ~src: (Type.Tuple [Type.Var "t0"; Type.Var "t0"])
      ~dst: (Type.Tuple [Type.Simple "Int"; Type.Simple "Str"])
     ~errors: [
        Typed.Unify.Mismatch {
          expected = Type.Simple "Int";
          got = Type.Simple "Str"
        }
      ]
      ~expect: []
      ()

  let test_tuple () = 
    test 
      ~src: (Type.Tuple [Type.Var "t0"])
      ~dst: (Type.Tuple [Type.Simple "Int"])
      ~expect: [
        "t0", Type.Simple "Int"
      ] ()
  
  let tests = [
    "var:src ok", `Quick, test_var_src_ok;
    "var:dst ok", `Quick, test_var_dst_ok;
    "var:simple mismatch", `Quick, test_simple_mismatch;
    "var:tuple mismatch", `Quick, test_tuple_mismatch;
    "var:simple ok", `Quick, test_simple_ok;
    "tuple", `Quick, test_tuple;
  ]

  let run_test () = Alcotest.run "Unify" [
    "InferTest", tests
  ]
  *)

module Infer = struct 
  let local_ident_expr name = Typed.Expr.Ident (Typed.Ident.{
    resolved = resolved name;
    resolution = [];
    range = Common.Span.empty_range;
    scheme = None;
  })

  let typed_local_ident_expr name typ = Typed.Expr.Ident (Typed.Ident.{
    resolved = resolved name;
    resolution = [];
    range = Common.Span.empty_range;
    scheme = Some (Typed.Type.make_scheme [] typ);
  })

  let typed_local_ident_expr name typ = Typed.Expr.Ident (Typed.Ident.{
    resolved = resolved name;
    resolution = [];
    range = Common.Span.empty_range;
    scheme = Some (Typed.Type.make_scheme [] typ);
  })

  let global_ident_expr name typ = Typed.Expr.Ident (Typed.Ident.{
    resolved = resolved name;
    resolution = [];
    range = Common.Span.empty_range;
    scheme = Some (Typed.Type.make_scheme [] typ);
  })

  let global_scheme_expr name typ = Typed.Expr.Ident (Typed.Ident.{
    resolved = resolved name;
    resolution = [];

    range = Common.Span.empty_range;
    scheme = Some typ;
  })

  let tuple_expr exprs = Typed.Expr.Tuple (Typed.Tuple.{
    range = Common.Span.empty_range;
    exprs = exprs
  })


  let test ?(errors=[]) ~node ~expect_type ~expect = 
    let ctx = Typed.Inferno.{
      tempvar = Typed.Type_util.make_tempvar_gen "t";
      errors = [];
      substs = Map.empty(module String);
      env = Map.empty(module String);
    } in
    let typ = Typed.Infer.expr ~ctx node in
    check_results ~ctx ~errors ~expect_type ~expect ctx.substs typ


  let test_apply () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        range = Common.Span.empty_range;
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [Typed.Base_types.int; simple "Str"];
        args = [
          typed_local_ident_expr "hello" @@ Typed.Type.Var "t0"
        ]
      })
      ~expect_type: (simple "Str")
      ~expect: [
        "t0", Typed.Base_types.int
      ]
      ~errors: []

  let test_simple_apply_mismatch () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        range = Common.Span.empty_range;
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [Typed.Base_types.int; simple "Str"];
        args = [
          typed_local_ident_expr "hello" @@ simple "Float"
        ]
      })
      ~expect_type: (simple "Str")
      ~errors: [
        Typed.Errors.TypeMismatch {
          range = Common.Span.empty_range;
          type_provided = simple "Float";
          type_expected = Typed.Base_types.int;
        }
      ]
      ~expect: []

  let test_inferred_apply_mismatch () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        range = Common.Span.empty_range;
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [Typed.Base_types.int; simple "Float"; simple "Str"];
        args = [
          typed_local_ident_expr "hello" @@ Typed.Type.Var "t0";
          typed_local_ident_expr "hello" @@ Typed.Type.Var "t0"
        ]
      })
      ~expect_type: (simple "Str")
      ~errors: [
        Typed.Errors.TypeMismatch {
          range = Common.Span.empty_range;
          type_provided = Typed.Base_types.int;
          type_expected = simple "Float";
        }
      ]
      ~expect: []

  let test_inferred_nested_apply_mismatch () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        range = Common.Span.empty_range;
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [
          simple "Str"; 
          Typed.Type.Tuple [
            Typed.Base_types.int;
            simple "Float";
          ];
          Typed.Base_types.int
        ];
        args = [
          typed_local_ident_expr "hello" @@ Typed.Type.Var "t0";
          tuple_expr [
            typed_local_ident_expr "hello" @@ Typed.Base_types.int;
            typed_local_ident_expr "hello" @@ Typed.Type.Var "t0";
          ]
        ]
      })
      ~expect_type: (Typed.Base_types.int)
      ~errors: [
        Typed.Errors.TypeMismatch {
          range = Common.Span.empty_range;
          type_provided = simple "Str";
          type_expected = simple "Float";
        }
      ]
      ~expect: []

  let test_inferred_dobule_nested_apply_mismatch () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        range = Common.Span.empty_range;
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [
          simple "Str"; 
          Typed.Type.Tuple [
            Typed.Base_types.int;
            Typed.Type.Tuple [
              Typed.Base_types.int;
              simple "Float";
            ]
          ];
          Typed.Base_types.int
        ];
        args = [
          typed_local_ident_expr "hello" @@ Typed.Type.Var "t0";
          tuple_expr [
            typed_local_ident_expr "hello" @@ Typed.Base_types.int;
            tuple_expr [
              typed_local_ident_expr "hello" @@ Typed.Base_types.int;
              typed_local_ident_expr "hello" @@ Typed.Type.Var "t0";
            ]
          ]
        ]
      })
      ~expect_type: (Typed.Base_types.int)
      ~errors: [
        Typed.Errors.TypeMismatch {
          range = Common.Span.empty_range;
          type_provided = simple "Str";
          type_expected = simple "Float";
        }
      ]
      ~expect: []

  let test_not_a_function () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        range = Common.Span.empty_range;
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [
          simple "Str"; 
          simple "Str"; 
        ];
        args = [
          typed_local_ident_expr "hello" @@ simple "Str";
          typed_local_ident_expr "hello" @@ simple "Str";
          typed_local_ident_expr "hello" @@ Typed.Base_types.int;
        ]
      })
      ~expect_type: (simple "Str")
      ~errors: [
        Typed.Errors.NotFunction {
          range = Common.Span.empty_range;
          type_provided = simple "Str";
        }
      ]
      ~expect: []

  let test_simple_generic () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        range = Common.Span.empty_range;
        fn = global_scheme_expr "test" @@ Typed.Type.lambda ~constr:["t"] [
          Typed.Type.Var "t"; 
          Typed.Type.Var "t"; 
        ];
        args = [
          typed_local_ident_expr "hello" @@ simple "Str";
        ]
      })
      ~expect_type: (simple "Str")
      ~errors: []
      ~expect: []

  let test_simple_generic_multiparam () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        range = Common.Span.empty_range;
        fn = global_scheme_expr "test" @@ Typed.Type.lambda ~constr:["t"; "u"] [
          Typed.Type.Var "t"; 
          Typed.Type.Var "u"; 
          Typed.Type.Tuple [Typed.Type.Var "t"; Typed.Type.Var "u"]; 
        ];
        args = [
          typed_local_ident_expr "str" @@ simple "Str";
          typed_local_ident_expr "int" @@ Typed.Base_types.int;
        ]
      })
      ~expect_type: (
          Typed.Type.Tuple [
            simple "Str";
            Typed.Base_types.int
          ] 
        )
      ~errors: []
      ~expect: []

  let test_simple_generic_mismatch () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        range = Common.Span.empty_range;
        fn = global_scheme_expr "test" @@ Typed.Type.lambda ~constr:["t"] [
          Typed.Type.Var "t"; 
          Typed.Type.Var "t"; 
          Typed.Type.Var "t"; 
        ];
        args = [
          typed_local_ident_expr "str" @@ simple "Str";
          typed_local_ident_expr "int" @@ Typed.Base_types.int;
        ]
      })
      ~expect_type: (simple "Str")
      ~errors: [
        Typed.Errors.TypeMismatch {
          range = Common.Span.empty_range;
          type_provided = Typed.Base_types.int;
          type_expected = simple "Str";
        }
      ]
      ~expect: []

  let test_lambda_infer () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        range = Common.Span.empty_range;
        fn = global_ident_expr "test" @@ Typed.Type.Var "a0";
        args = [
          typed_local_ident_expr "str" @@ simple "Str";
          typed_local_ident_expr "int" @@ Typed.Base_types.int;
        ]
      })
      ~expect_type: (
        (Typed.Type.lambda [
          Typed.Type.Var "t0"
        ]).typ
      )
      ~errors: []
      ~expect: [
        "a0", (Typed.Type.lambda [
          simple "Str";
          Typed.Base_types.int;
          Typed.Type.Var "t0"
        ]).typ

      ]

  let test_func_one_arg () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        range = Common.Span.empty_range;
        fn = global_ident_expr "int_to_int" @@ (Typed.Type.lambda [
          Typed.Base_types.int;
          Typed.Base_types.int;
        ]).typ;
        args = [
          Typed.Expr.Apply (Typed.Apply.{
            range = Common.Span.empty_range;
            fn = global_ident_expr "fn" @@ Typed.Type.Var "a0";
            args = [
              typed_local_ident_expr "int" @@ Typed.Base_types.int;
            ];
          })
        ]
      })
      ~expect_type: (
        Typed.Base_types.int;
      )
      ~errors: []
      ~expect: [
        "a0", (Typed.Type.lambda [
          Typed.Base_types.int;
          Typed.Base_types.int
        ]).typ
      ]

  let test_func_arg_mismatch () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        range = Common.Span.empty_range;
        fn = global_ident_expr "+" @@ (Typed.Type.lambda [
          Typed.Base_types.int;
          Typed.Base_types.int;
          Typed.Base_types.int;
        ]).typ;
        args = [
          Typed.Expr.Apply (Typed.Apply.{
            range = Common.Span.empty_range;
            fn = global_ident_expr "fn" @@ Typed.Type.Var "a0";
            args = [
              typed_local_ident_expr "int" @@ Typed.Base_types.int;
            ];
          });
          Typed.Expr.Apply (Typed.Apply.{
            range = Common.Span.empty_range;
            fn = global_ident_expr "fn" @@ Typed.Type.Var "a0";
            args = [
              typed_local_ident_expr "str" @@ simple "Str";
            ];
          })
        ]
      })
      ~expect_type: (
        Typed.Base_types.int;
      )
      ~errors: [
        Typed.Errors.TypeMismatch {
          range = Common.Span.empty_range;
          type_provided = simple "Str";
          type_expected = Typed.Base_types.int;
        }
      ]
      ~expect: [
        "a0", (Typed.Type.lambda [
          Typed.Base_types.int;
          Typed.Base_types.int
        ]).typ
      ]

  let tests = [
    "var:apply", `Quick, test_apply;
    "apply mismatch", `Quick, test_simple_apply_mismatch;
    "no a function", `Quick, test_not_a_function;
    "inferred nested mismatch", `Quick, test_inferred_nested_apply_mismatch;
    "inferred nested double mismatch", `Quick, test_inferred_dobule_nested_apply_mismatch;
    "simple generic", `Quick, test_simple_generic;
    "simple generic multiparam", `Quick, test_simple_generic_multiparam;
    "simple generic mismtach", `Quick, test_simple_generic_mismatch;
    "lambda infer", `Quick, test_lambda_infer;
    "one arg", `Quick, test_func_one_arg;
    "function arg mismatch", `Quick, test_func_arg_mismatch
  ]

  module Let = struct 
  end

  module Block = struct 
  end

  let single_expr_block expr = Typed.Block.{stmts = [Typed.Stmt.Expr expr]; range = Common.Span.empty_range}
  let unit_expr = Typed.Expr.Tuple (Typed.Tuple.{exprs = []; range = Common.Span.empty_range})
  module Cond = struct 
    let no_else_ok () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        range = Common.Span.empty_range;
        cases = [Typed.Cond.{
          if_ = single_expr_block @@ typed_local_ident_expr "True" @@ Typed.Base_types.bool;
          then_ = single_expr_block @@ unit_expr;
        }];
        else_ = None
      }))
      ~expect_type: (
        Typed.Base_types.unit;
      )
      ~errors: []
      ~expect: []

    let no_else_if_mismatch () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        range = Common.Span.empty_range;
        cases = [Typed.Cond.{
          if_ = single_expr_block @@ typed_local_ident_expr "10" @@ Typed.Base_types.int;
          then_ = single_expr_block @@ unit_expr;
        }];
        else_ = None
      }))
      ~expect_type: (
        Typed.Base_types.unit;
      )
      ~errors: [
        Typed.Errors.IfTypeMismatch {
          range = Common.Span.empty_range;
          unexpected = Typed.Base_types.int
        }
      ]
      ~expect: []

    let no_else_if_tuple_mismatch () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        range = Common.Span.empty_range;
        cases = [Typed.Cond.{
          if_ = single_expr_block @@ tuple_expr [
            typed_local_ident_expr "10" @@ Typed.Base_types.int;
            typed_local_ident_expr "10" @@ Typed.Base_types.int;
          ];
          then_ = single_expr_block @@ unit_expr;
        }];
        else_ = None
      }))
      ~expect_type: (
        Typed.Base_types.unit;
      )
      ~errors: [
        Typed.Errors.IfTypeMismatch {
          range = Common.Span.empty_range;
          unexpected = Typed.Type.Tuple [Typed.Base_types.int; Typed.Base_types.int]
        }
      ]
      ~expect: []

    let no_else_then_mismatch () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        range = Common.Span.empty_range;
        cases = [Typed.Cond.{
          if_ = single_expr_block @@ tuple_expr [
            typed_local_ident_expr "x" @@ Typed.Base_types.bool;
          ];
          then_ = single_expr_block @@ typed_local_ident_expr "10" @@ (Typed.Base_types.int);
        }];
        else_ = None
      }))
      ~expect_type: (
        Typed.Base_types.unit;
      )
      ~errors: [
        Typed.Errors.BranchTypeMismatch {
          range = Common.Span.empty_range;
          unexpected = Typed.Base_types.int;
          expected = Typed.Base_types.unit
        }
      ]
      ~expect: []

    let ternary_else_mismatch () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        range = Common.Span.empty_range;
        cases = [Typed.Cond.{
          if_ = single_expr_block @@ tuple_expr [
            typed_local_ident_expr "x" @@ Typed.Base_types.bool;
          ];
          then_ = single_expr_block @@ typed_local_ident_expr "10" @@ (Typed.Base_types.int);
        }];
        else_ = Some (single_expr_block @@ typed_local_ident_expr "hello" @@ (Typed.Base_types.str));
      }))
      ~expect_type: (
        Typed.Base_types.int;
      )
      ~errors: [
        Typed.Errors.BranchTypeMismatch {
          range = Common.Span.empty_range;
          unexpected = Typed.Base_types.str;
          expected = Typed.Base_types.int
        }
      ]
      ~expect: []

    let multi_cond_second_then_mismatch () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
         range = Common.Span.empty_range;
        cases = [
          Typed.Cond.{
            if_ = single_expr_block @@ tuple_expr [
              typed_local_ident_expr "x" @@ Typed.Base_types.bool;
            ];
            then_ = single_expr_block @@ typed_local_ident_expr "10" @@ (Typed.Base_types.int);
          };
          Typed.Cond.{
            if_ = single_expr_block @@ tuple_expr [
              typed_local_ident_expr "x" @@ Typed.Base_types.bool;
            ];
            then_ = single_expr_block @@ typed_local_ident_expr "10" @@ (Typed.Base_types.str);
          }
        ];
        else_ = Some (single_expr_block @@ typed_local_ident_expr "hello" @@ (Typed.Base_types.int));
      }))
      ~expect_type: (
        Typed.Base_types.int;
      )
      ~errors: [
        Typed.Errors.BranchTypeMismatch {
          range = Common.Span.empty_range;
          unexpected = Typed.Base_types.str;
          expected = Typed.Base_types.int
        }
      ]
      ~expect: []

    let multi_cond_else_mismatch () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        range = Common.Span.empty_range;
        cases = [
          Typed.Cond.{
            if_ = single_expr_block @@ tuple_expr [
              typed_local_ident_expr "x" @@ Typed.Base_types.bool;
            ];
            then_ = single_expr_block @@ typed_local_ident_expr "10" @@ (Typed.Base_types.int);
          };
          Typed.Cond.{
            if_ = single_expr_block @@ tuple_expr [
              typed_local_ident_expr "x" @@ Typed.Base_types.bool;
            ];
            then_ = single_expr_block @@ typed_local_ident_expr "10" @@ (Typed.Base_types.int);
          }
        ];
        else_ = Some (single_expr_block @@ typed_local_ident_expr "hello" @@ (Typed.Base_types.str));
      }))
      ~expect_type: (
        Typed.Base_types.int;
      )
      ~errors: [
        Typed.Errors.BranchTypeMismatch {
          range = Common.Span.empty_range;
          unexpected = Typed.Base_types.str;
          expected = Typed.Base_types.int
        }
      ]
      ~expect: []


    let tests = [
      "no_else:ok", `Quick, no_else_ok;
      "no_else:if_mismatch", `Quick, no_else_if_mismatch;
      "no_else:if_tuple_mismatch", `Quick, no_else_if_tuple_mismatch;
      "no_else:then_mismatch", `Quick, no_else_then_mismatch;
      "ternary:else_mismatch", `Quick, ternary_else_mismatch;
      "multi_cond:second_then_mismatch", `Quick, multi_cond_second_then_mismatch;
      "multi_cond:else_mismatch", `Quick, multi_cond_else_mismatch;
      (*"if_else:second_then_mismatch", `Quick, todo;
      "if_else:else_mismatch", `Quick, todo;

      "ok:without_else", `Quick, todo;
      "err:expect_unit_without_else", `Quick, todo;*)
    ]
  end

  let run_test () = Alcotest.run "Typed" [
    (*"Block", Block.tests;
    "Cond", Cond.tests;
    "InferTest", tests;
    "Let", Let.tests;
    "Resolve", Resolve_test.tests;*)
    (* "Block", Block_infer_test.tests *)
    (* "Toplevel", Toplevel_infer_test.tests; *)
    (* "Import", Import_test.tests; *)
    "Global", Global_infer_test.tests;
  ]
  (* Todo Rest: 
    - Block (inference errors)
    - Cond 
    - Lambda (makes correct schema)
    - Let
  *)
end *)

let tests = [
  (* "Unify", Unify.tests; *)
  "Infer", Typed_infer_test.tests;
  "Type", Typed_test__type.tests;
  "Misc", Typed_test__misc.tests;
  (* "Typed:Let", Typed_test__let.tests;
  "Typed:match", Typed_test__match.tests; *)
  (* "Block", Typed_test__infer__block.tests;
  "Cond", Infer.Cond.tests;
  "Infer", Infer.tests;
  "Global", Global_infer_test.tests *)
]

(* let () = Infer.run_test () *)