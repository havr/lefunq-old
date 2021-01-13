module Fake = struct 
  let local_ident name type_ = 
    Typed.Ident.{
      pos = Common.Pos.empty;
      given_name = name;
      scheme = Some (Typed.Type.make_scheme [] type_); (* TODO: is it really needed here *)
      resolved = Typed.Ident.Local {
        scope_name = name;
        param = false;
      }
    }

  let global_ident name type_ = 
    Typed.Ident.{
      pos = Common.Pos.empty;
      given_name = name;
      scheme = Some (Typed.Type.make_scheme [] type_); (* TODO: is it really needed here *)
      resolved = Typed.Ident.Global {
        global_name = name
      }
    }
end

let test_todo () = Alcotest.fail "todo"

open Base

let difference ~equals src check = 
  List.fold src ~init: ([]) ~f: (fun a x ->
    match List.find check ~f: (equals x) with
    | Some _ -> a
    | None -> x :: a
  )

module Unify = struct 
  open Typed

  let results expect substs = List.filter_map expect ~f: (fun (var', subst) ->
    match Map.find substs var' with
    | Some t ->
      if not @@ Type.equals t subst then begin
        let expect_str = Type.to_string subst in
        let got_str = Type.to_string t in
        Some (var' ^ ": " ^ got_str ^ " != " ^ expect_str)
      end else None
    | None -> 
      Some (var' ^ ": no substitution")
  )
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
end

module Infer = struct 
  let local_ident_expr name = Typed.Expr.Ident (Typed.Ident.{
    pos = Common.Pos.empty;
    given_name = name;
    resolved = Typed.Ident.Local {scope_name = name; param = false};
    scheme = None;
  })

  let typed_local_ident_expr name typ = Typed.Expr.Ident (Typed.Ident.{
    pos = Common.Pos.empty;
    given_name = name;
    resolved = Typed.Ident.Local {scope_name = name; param = false};
    scheme = Some (Typed.Type.make_scheme [] typ);
  })

  let global_ident_expr name typ = Typed.Expr.Ident (Typed.Ident.{
    pos = Common.Pos.empty;
    given_name = name;
    resolved = Typed.Ident.Global {global_name = name};
    scheme = Some (Typed.Type.make_scheme [] typ);
  })

  let global_scheme_expr name typ = Typed.Expr.Ident (Typed.Ident.{
    pos = Common.Pos.empty;
    given_name = name;
    resolved = Typed.Ident.Global {global_name = name};
    scheme = Some typ;
  })

  let tuple_expr exprs = Typed.Expr.Tuple (Typed.Tuple.{
    exprs = exprs
  })

  let check_results ?(errors=[]) ~ctx ~expect_type ~expect subst typ = 
    let typ = Typed.Subst.apply_substs subst typ in
    let results = Unify.results expect subst in
    begin 
      if not @@ Typed.Type.equals typ expect_type then
        Alcotest.fail @@ String.concat [
          "(got) "; Typed.Type.to_string typ; " != (expected) "; Typed.Type.to_string expect_type
        ]
    end;
    begin 
      if List.length results > 0 then
        Alcotest.fail (String.concat ~sep: "\n" results)
    end;
    begin 
        let errors_expected_not_found = difference 
          ~equals: (Typed.Error.equals) errors (Typed.Basket.get Typed.Infer.(ctx.errors)) in
        let unexpected_got = difference 
          ~equals: (Typed.Error.equals) (Typed.Basket.get ctx.errors) errors in
        let map_errors errors = errors |> List.map ~f:Typed.Error.to_string in
        let exp_not_found = if List.length errors_expected_not_found > 0 then
          map_errors errors_expected_not_found 
          |> String.concat ~sep: "\n" else "" in
        let unexp = if List.length unexpected_got > 0 then
          map_errors unexpected_got 
          |> String.concat ~sep: "\n" else "" in
        let labeled_exp = if String.is_empty exp_not_found then
          "" else "expected error: " ^ exp_not_found in
        let labeled_unexp = if String.is_empty unexp then
          "" else "unexpected error: " ^ unexp in

        let issues = [labeled_exp; labeled_unexp] 
        |> List.filter ~f: (fun s -> not @@ String.is_empty s) in
        if List.length issues > 0 then
          issues 
            |> String.concat ~sep: "\n"
            |> Alcotest.fail
            |> ignore
      end

  let test ?(errors=[]) ~node ~expect_type ~expect = 
    let ctx = Typed.Infer.{
      errors = Typed.Basket.make ();
      tempvar = Typed.make_tempvar_gen "t";
      store = Typed.TypeStore.make ();
    } in
    let (subst, typ) = Typed.Infer.expr ~ctx (Map.empty (module String)) (Typed.Subst.empty_subst) node in
    check_results ~ctx ~errors ~expect_type ~expect subst typ


  let test_apply () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [Typed.BaseTypes.int; Typed.Type.Simple "Str"];
        args = [
          typed_local_ident_expr "hello" @@ Typed.Type.Var "t0"
        ]
      })
      ~expect_type: (Typed.Type.Simple "Str")
      ~expect: [
        "t0", Typed.BaseTypes.int
      ]
      ~errors: []

  let test_simple_apply_mismatch () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [Typed.BaseTypes.int; Typed.Type.Simple "Str"];
        args = [
          typed_local_ident_expr "hello" @@ Typed.Type.Simple "Float"
        ]
      })
      ~expect_type: (Typed.Type.Simple "Str")
      ~errors: [
        Typed.Error.TypeMismatch {
          type_provided = Typed.Type.Simple "Float";
          type_expected = Typed.BaseTypes.int;
        }
      ]
      ~expect: []

  let test_inferred_apply_mismatch () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [Typed.BaseTypes.int; Typed.Type.Simple "Float"; Typed.Type.Simple "Str"];
        args = [
          typed_local_ident_expr "hello" @@ Typed.Type.Var "t0";
          typed_local_ident_expr "hello" @@ Typed.Type.Var "t0"
        ]
      })
      ~expect_type: (Typed.Type.Simple "Str")
      ~errors: [
        Typed.Error.TypeMismatch {
          type_provided = Typed.BaseTypes.int;
          type_expected = Typed.Type.Simple "Float";
        }
      ]
      ~expect: []

  let test_inferred_nested_apply_mismatch () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [
          Typed.Type.Simple "Str"; 
          Typed.Type.Tuple [
            Typed.BaseTypes.int;
            Typed.Type.Simple "Float";
          ];
          Typed.BaseTypes.int
        ];
        args = [
          typed_local_ident_expr "hello" @@ Typed.Type.Var "t0";
          tuple_expr [
            typed_local_ident_expr "hello" @@ Typed.BaseTypes.int;
            typed_local_ident_expr "hello" @@ Typed.Type.Var "t0";
          ]
        ]
      })
      ~expect_type: (Typed.BaseTypes.int)
      ~errors: [
        Typed.Error.TypeMismatch {
          type_provided = Typed.Type.Simple "Str";
          type_expected = Typed.Type.Simple "Float";
        }
      ]
      ~expect: []

  let test_inferred_dobule_nested_apply_mismatch () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [
          Typed.Type.Simple "Str"; 
          Typed.Type.Tuple [
            Typed.BaseTypes.int;
            Typed.Type.Tuple [
              Typed.BaseTypes.int;
              Typed.Type.Simple "Float";
            ]
          ];
          Typed.BaseTypes.int
        ];
        args = [
          typed_local_ident_expr "hello" @@ Typed.Type.Var "t0";
          tuple_expr [
            typed_local_ident_expr "hello" @@ Typed.BaseTypes.int;
            tuple_expr [
              typed_local_ident_expr "hello" @@ Typed.BaseTypes.int;
              typed_local_ident_expr "hello" @@ Typed.Type.Var "t0";
            ]
          ]
        ]
      })
      ~expect_type: (Typed.BaseTypes.int)
      ~errors: [
        Typed.Error.TypeMismatch {
          type_provided = Typed.Type.Simple "Str";
          type_expected = Typed.Type.Simple "Float";
        }
      ]
      ~expect: []

  let test_not_a_function () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [
          Typed.Type.Simple "Str"; 
          Typed.Type.Simple "Str"; 
        ];
        args = [
          typed_local_ident_expr "hello" @@ Typed.Type.Simple "Str";
          typed_local_ident_expr "hello" @@ Typed.Type.Simple "Str";
          typed_local_ident_expr "hello" @@ Typed.BaseTypes.int;
        ]
      })
      ~expect_type: (Typed.Type.Simple "Str")
      ~errors: [
        Typed.Error.NotFunction {
          type_provided = Typed.Type.Simple "Str";
        }
      ]
      ~expect: []

  let test_simple_generic () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda ~constr:["t"] [
          Typed.Type.Var "t"; 
          Typed.Type.Var "t"; 
        ];
        args = [
          typed_local_ident_expr "hello" @@ Typed.Type.Simple "Str";
        ]
      })
      ~expect_type: (Typed.Type.Simple "Str")
      ~errors: []
      ~expect: []

  let test_simple_generic_multiparam () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda ~constr:["t"; "u"] [
          Typed.Type.Var "t"; 
          Typed.Type.Var "u"; 
          Typed.Type.Tuple [Typed.Type.Var "t"; Typed.Type.Var "u"]; 
        ];
        args = [
          typed_local_ident_expr "str" @@ Typed.Type.Simple "Str";
          typed_local_ident_expr "int" @@ Typed.BaseTypes.int;
        ]
      })
      ~expect_type: (
          Typed.Type.Tuple [
            Typed.Type.Simple "Str";
            Typed.BaseTypes.int
          ] 
        )
      ~errors: []
      ~expect: []

  let test_simple_generic_mismatch () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda ~constr:["t"] [
          Typed.Type.Var "t"; 
          Typed.Type.Var "t"; 
          Typed.Type.Var "t"; 
        ];
        args = [
          typed_local_ident_expr "str" @@ Typed.Type.Simple "Str";
          typed_local_ident_expr "int" @@ Typed.BaseTypes.int;
        ]
      })
      ~expect_type: (Typed.Type.Simple "Str")
      ~errors: [
        Typed.Error.TypeMismatch {
          type_provided = Typed.BaseTypes.int;
          type_expected = Typed.Type.Simple "Str";
        }
      ]
      ~expect: []

  let test_lambda_infer () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        fn = global_ident_expr "test" @@ Typed.Type.Var "a0";
        args = [
          typed_local_ident_expr "str" @@ Typed.Type.Simple "Str";
          typed_local_ident_expr "int" @@ Typed.BaseTypes.int;
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
          Typed.Type.Simple "Str";
          Typed.BaseTypes.int;
          Typed.Type.Var "t0"
        ]).typ

      ]

  let test_func_one_arg () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        fn = global_ident_expr "int_to_int" @@ (Typed.Type.lambda [
          Typed.BaseTypes.int;
          Typed.BaseTypes.int;
        ]).typ;
        args = [
          Typed.Expr.Apply (Typed.Apply.{
            fn = global_ident_expr "fn" @@ Typed.Type.Var "a0";
            args = [
              typed_local_ident_expr "int" @@ Typed.BaseTypes.int;
            ];
          })
        ]
      })
      ~expect_type: (
        Typed.BaseTypes.int;
      )
      ~errors: []
      ~expect: [
        "a0", (Typed.Type.lambda [
          Typed.BaseTypes.int;
          Typed.BaseTypes.int
        ]).typ
      ]

  let test_func_arg_mismatch () = test 
      ~node: (Typed.Expr.Apply Typed.Apply.{
        fn = global_ident_expr "+" @@ (Typed.Type.lambda [
          Typed.BaseTypes.int;
          Typed.BaseTypes.int;
          Typed.BaseTypes.int;
        ]).typ;
        args = [
          Typed.Expr.Apply (Typed.Apply.{
            fn = global_ident_expr "fn" @@ Typed.Type.Var "a0";
            args = [
              typed_local_ident_expr "int" @@ Typed.BaseTypes.int;
            ];
          });
          Typed.Expr.Apply (Typed.Apply.{
            fn = global_ident_expr "fn" @@ Typed.Type.Var "a0";
            args = [
              typed_local_ident_expr "str" @@ Typed.Type.Simple "Str";
            ];
          })
        ]
      })
      ~expect_type: (
        Typed.BaseTypes.int;
      )
      ~errors: [
        Typed.Error.TypeMismatch {
          type_provided = Typed.Type.Simple "Str";
          type_expected = Typed.BaseTypes.int;
        }
      ]
      ~expect: [
        "a0", (Typed.Type.lambda [
          Typed.BaseTypes.int;
          Typed.BaseTypes.int
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
    open Typed
    (* TODO: open Typed *)
    (* TODO: use test block *)
    let test ?(errors=[]) ~stmts ~expect_type ~expect = 
      let ctx = Typed.Infer.{
        errors = Typed.Basket.make ();
        tempvar = Typed.make_tempvar_gen "t";
        store = Typed.TypeStore.make ();
      } in
      let (subst, typ) = Typed.Infer.block ~ctx (Map.empty (module String)) (Typed.Subst.empty_subst) (Typed.Block.{stmts = stmts}) in
      check_results ~ctx ~errors ~expect_type ~expect subst typ

    open Helper

    let let_single () = test
      ~stmts: [
        let_stmt "hello" [
          value_stmt "10" (BaseTypes.int)
        ];
        local_ident_stmt "hello" 
      ]
      ~expect_type: (
        Typed.Type.Simple BaseTypes.int_name;
      )
      ~errors: []
      ~expect: []
      

    let let_lambda () = test
      ~stmts: [
        let_stmt "hello" [
          lambda_stmt ["a", Type.Var "t"] [
            local_ident_stmt ~scheme:(Type.make_scheme [] (Type.Var "t")) "a"
          ]
        ];
        local_ident_stmt "hello"
      ]
      ~expect_type: (
        (Type.lambda [Type.Var "t"; Type.Var "t"]).typ;
      )
      ~errors: []
      ~expect: []

    let tests = [
      "let:single", `Quick, let_single;
      "let:lambda", `Quick, let_lambda;
    ]
  end

  module Block = struct 
    let test ?(errors=[]) ~stmts ~expect_type ~expect = 
      let ctx = Typed.Infer.{
        errors = Typed.Basket.make ();
        tempvar = Typed.make_tempvar_gen "t";
        store = Typed.TypeStore.make ();
      } in
      let (subst, typ) = Typed.Infer.block ~ctx (Map.empty (module String)) (Typed.Subst.empty_subst) (Typed.Block.{stmts = stmts}) in
      check_results ~ctx ~errors ~expect_type ~expect subst typ
      
    let test_returns_last_statement () = test
      ~stmts: [
        Typed.Stmt.Expr (Typed.Expr.Tuple (Typed.Tuple.{exprs=[]}));
        Typed.Stmt.Expr (Typed.Expr.Value (Typed.Value.{value="10"; type_=Typed.BaseTypes.int}))
      ]
      ~expect_type: (
        Typed.BaseTypes.int;
      )
      ~errors: []
      ~expect: []

    let test_not_unit_result_error () = test
      ~stmts: [
        Typed.Stmt.Expr (Typed.Expr.Value (Typed.Value.{value="hello"; type_=Typed.BaseTypes.str}));
        Typed.Stmt.Expr (Typed.Expr.Value (Typed.Value.{value="10"; type_=Typed.BaseTypes.int}))
      ]
      ~expect_type: (
        Typed.BaseTypes.int;
      )
      ~errors: [
        Typed.Error.IgnoredResult {
          type_provided = Typed.BaseTypes.str;
        }
      ]
      ~expect: []

    let tests = [
      "ignored result", `Quick, test_not_unit_result_error;
      "last statement", `Quick, test_returns_last_statement
    ]
  end

  let single_expr_block expr = Typed.Block.{stmts = [Typed.Stmt.Expr expr]}
  let unit_expr = Typed.Expr.Tuple (Typed.Tuple.{exprs = []})
  module Cond = struct 
    let no_else_ok () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        cases = [Typed.Cond.{
          if_ = single_expr_block @@ typed_local_ident_expr "True" @@ Typed.BaseTypes.bool;
          then_ = single_expr_block @@ unit_expr;
        }];
        else_ = None
      }))
      ~expect_type: (
        Typed.BaseTypes.unit;
      )
      ~errors: []
      ~expect: []

    let no_else_if_mismatch () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        cases = [Typed.Cond.{
          if_ = single_expr_block @@ typed_local_ident_expr "10" @@ Typed.BaseTypes.int;
          then_ = single_expr_block @@ unit_expr;
        }];
        else_ = None
      }))
      ~expect_type: (
        Typed.BaseTypes.unit;
      )
      ~errors: [
        Typed.Error.IfTypeMismatch {unexpected = Typed.BaseTypes.int}
      ]
      ~expect: []

    let no_else_if_tuple_mismatch () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        cases = [Typed.Cond.{
          if_ = single_expr_block @@ tuple_expr [
            typed_local_ident_expr "10" @@ Typed.BaseTypes.int;
            typed_local_ident_expr "10" @@ Typed.BaseTypes.int;
          ];
          then_ = single_expr_block @@ unit_expr;
        }];
        else_ = None
      }))
      ~expect_type: (
        Typed.BaseTypes.unit;
      )
      ~errors: [
        Typed.Error.IfTypeMismatch {unexpected = Typed.Type.Tuple [Typed.BaseTypes.int; Typed.BaseTypes.int]}
      ]
      ~expect: []

    let no_else_then_mismatch () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        cases = [Typed.Cond.{
          if_ = single_expr_block @@ tuple_expr [
            typed_local_ident_expr "x" @@ Typed.BaseTypes.bool;
          ];
          then_ = single_expr_block @@ typed_local_ident_expr "10" @@ (Typed.BaseTypes.int);
        }];
        else_ = None
      }))
      ~expect_type: (
        Typed.BaseTypes.unit;
      )
      ~errors: [
        Typed.Error.BranchTypeMismatch {unexpected = Typed.BaseTypes.int; expected = Typed.BaseTypes.unit}
      ]
      ~expect: []

    let ternary_else_mismatch () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        cases = [Typed.Cond.{
          if_ = single_expr_block @@ tuple_expr [
            typed_local_ident_expr "x" @@ Typed.BaseTypes.bool;
          ];
          then_ = single_expr_block @@ typed_local_ident_expr "10" @@ (Typed.BaseTypes.int);
        }];
        else_ = Some (single_expr_block @@ typed_local_ident_expr "hello" @@ (Typed.BaseTypes.str));
      }))
      ~expect_type: (
        Typed.BaseTypes.int;
      )
      ~errors: [
        Typed.Error.BranchTypeMismatch {unexpected = Typed.BaseTypes.str; expected = Typed.BaseTypes.int}
      ]
      ~expect: []

    let multi_cond_second_then_mismatch () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        cases = [
          Typed.Cond.{
            if_ = single_expr_block @@ tuple_expr [
              typed_local_ident_expr "x" @@ Typed.BaseTypes.bool;
            ];
            then_ = single_expr_block @@ typed_local_ident_expr "10" @@ (Typed.BaseTypes.int);
          };
          Typed.Cond.{
            if_ = single_expr_block @@ tuple_expr [
              typed_local_ident_expr "x" @@ Typed.BaseTypes.bool;
            ];
            then_ = single_expr_block @@ typed_local_ident_expr "10" @@ (Typed.BaseTypes.str);
          }
        ];
        else_ = Some (single_expr_block @@ typed_local_ident_expr "hello" @@ (Typed.BaseTypes.int));
      }))
      ~expect_type: (
        Typed.BaseTypes.int;
      )
      ~errors: [
        Typed.Error.BranchTypeMismatch {unexpected = Typed.BaseTypes.str; expected = Typed.BaseTypes.int}
      ]
      ~expect: []

    let multi_cond_else_mismatch () = test
      ~node: (Typed.Expr.Cond (Typed.Cond.{
        cases = [
          Typed.Cond.{
            if_ = single_expr_block @@ tuple_expr [
              typed_local_ident_expr "x" @@ Typed.BaseTypes.bool;
            ];
            then_ = single_expr_block @@ typed_local_ident_expr "10" @@ (Typed.BaseTypes.int);
          };
          Typed.Cond.{
            if_ = single_expr_block @@ tuple_expr [
              typed_local_ident_expr "x" @@ Typed.BaseTypes.bool;
            ];
            then_ = single_expr_block @@ typed_local_ident_expr "10" @@ (Typed.BaseTypes.int);
          }
        ];
        else_ = Some (single_expr_block @@ typed_local_ident_expr "hello" @@ (Typed.BaseTypes.str));
      }))
      ~expect_type: (
        Typed.BaseTypes.int;
      )
      ~errors: [
        Typed.Error.BranchTypeMismatch {unexpected = Typed.BaseTypes.str; expected = Typed.BaseTypes.int}
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

  let run_test () = Alcotest.run "Infer" [
    (*"Block", Block.tests;
    "Cond", Cond.tests;
    "InferTest", tests;
    "Let", Let.tests;
    "Resolve", Resolve_test.tests;*)
    (* "Block", Block_infer_test.tests *)
    (* "Toplevel", Toplevel_infer_test.tests; *)
    "Global", Global_infer_test.tests;
  ]
  (* Todo Rest: 
    - Block (inference errors)
    - Cond 
    - Lambda (makes correct schema)
    - Let
  *)
end

let () = Infer.run_test ()