module Fake = struct 
  let local_ident name type_ = 
    Typed.Ident.{
      pos = Common.Pos.empty;
      given_name = name;
      type_ = type_; (* TODO: is it really needed here *)
      resolved = Typed.Ident.Local {
        scope_name = name;
        param = false;
      }
    }

  let global_ident name type_ = 
    Typed.Ident.{
      pos = Common.Pos.empty;
      given_name = name;
      type_ = type_; (* TODO: is it really needed here *)
      resolved = Typed.Ident.Global {
        global_name = name
      }
    }
end

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

  let test ~src ~dst ?(errors=[]) ~expect () =
    let ctx = Typed.Unify.make_ctx () in
    let substs = Typed.Unify.unify ~ctx src dst in
    let results = results expect substs
    in begin if List.length results > 0 then
        Alcotest.fail (String.concat ~sep: "\n" results) end;
    begin 
        let errors_expected_not_found = difference 
          ~equals: (Unify.error_equals) errors (Basket.get ctx.errors) in
        let unexpected_got = difference 
          ~equals: (Unify.error_equals) (Basket.get ctx.errors) errors in
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
      ~dst: (Type.Simple "Str")
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
end

module Infer = struct 
  let local_ident_expr name typ = Typed.Expr.Ident (Typed.Ident.{
    pos = Common.Pos.empty;
    given_name = "test";
    resolved = Typed.Ident.Local {scope_name = name; param = false};
    type_ = Typed.Type.{typ = typ; constr = []}
  })

  let global_ident_expr name typ = Typed.Expr.Ident (Typed.Ident.{
    pos = Common.Pos.empty;
    given_name = "test";
    resolved = Typed.Ident.Global {global_name = name};
    type_ = {typ = typ; constr = []}
  })

  let global_scheme_expr name typ = Typed.Expr.Ident (Typed.Ident.{
    pos = Common.Pos.empty;
    given_name = "test";
    resolved = Typed.Ident.Global {global_name = name};
    type_ = typ
  })

  let tuple_expr exprs = Typed.Expr.Tuple (Typed.Tuple.{
    exprs = exprs
  })

open Common

  let test ?(errors=[]) ~node ~expect_type ~expect= 
    let ctx = Typed.Infer.{
      errors = Typed.Basket.make ();
      namer = Typed.TypeNamer.make ();
      store = Typed.TypeStore.make ();
    } in
    let (subst, typ) = Typed.Infer.expr ~ctx (Typed.Infer.empty_subst) (Typed.Expr.Apply node) in
    Stdio.print_endline @@ "^" ^ Typed.Type.to_string typ;
    dbg [Typed.Infer.substs_to_string subst; Typed.Type.to_string typ];
    let typ = Typed.Infer.apply_substs subst typ in
    let results = Unify.results expect subst in
    begin 
      if not @@ Typed.Type.equals typ expect_type then
        Alcotest.fail (Typed.Type.to_string typ ^ "!=" ^ (Typed.Type.to_string expect_type))
    end;
    begin 
      if List.length results > 0 then
        Alcotest.fail (String.concat ~sep: "\n" results)
    end;
    begin 
        let errors_expected_not_found = difference 
          ~equals: (Typed.Infer.error_equals) errors (Typed.Basket.get ctx.errors) in
        let unexpected_got = difference 
          ~equals: (Typed.Infer.error_equals) (Typed.Basket.get ctx.errors) errors in
        let map_errors errors = errors |> List.map ~f: (fun err ->
          match err with
          | Typed.Infer.TypeMismatch {type_expected; type_provided} -> 
            "expected:" ^ (Typed.Type.to_string type_expected) ^ " != provided:" ^ (Typed.Type.to_string type_provided)
          | Typed.Infer.NotFunction {type_provided} -> 
            "not a function: " ^ (Typed.Type.to_string type_provided)
        ) in
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


  let test_apply () = test 
      ~node: Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [Typed.Type.Simple "Int"; Typed.Type.Simple "Str"];
        args = [
          local_ident_expr "hello" @@ Typed.Type.Var "t0"
        ]
      }
      ~expect_type: (Typed.Type.Simple "Str")
      ~expect: [
        "t0", Typed.Type.Simple "Int"
      ]
      ~errors: []

  let test_simple_apply_mismatch () = test 
      ~node: Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [Typed.Type.Simple "Int"; Typed.Type.Simple "Str"];
        args = [
          local_ident_expr "hello" @@ Typed.Type.Simple "Float"
        ]
      }
      ~expect_type: (Typed.Type.Simple "Str")
      ~errors: [
        Typed.Infer.TypeMismatch {
          type_provided = Typed.Type.Simple "Float";
          type_expected = Typed.Type.Simple "Int";
        }
      ]
      ~expect: []

  let test_inferred_apply_mismatch () = test 
      ~node: Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [Typed.Type.Simple "Int"; Typed.Type.Simple "Float"; Typed.Type.Simple "Str"];
        args = [
          local_ident_expr "hello" @@ Typed.Type.Var "t0";
          local_ident_expr "hello" @@ Typed.Type.Var "t0"
        ]
      }
      ~expect_type: (Typed.Type.Simple "Str")
      ~errors: [
        Typed.Infer.TypeMismatch {
          type_provided = Typed.Type.Simple "Int";
          type_expected = Typed.Type.Simple "Float";
        }
      ]
      ~expect: []

  let test_inferred_nested_apply_mismatch () = test 
      ~node: Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [
          Typed.Type.Simple "Str"; 
          Typed.Type.Tuple [
            Typed.Type.Simple "Int";
            Typed.Type.Simple "Float";
          ];
          Typed.Type.Simple "Int"
        ];
        args = [
          local_ident_expr "hello" @@ Typed.Type.Var "t0";
          tuple_expr [
            local_ident_expr "hello" @@ Typed.Type.Simple "Int";
            local_ident_expr "hello" @@ Typed.Type.Var "t0";
          ]
        ]
      }
      ~expect_type: (Typed.Type.Simple "Int")
      ~errors: [
        Typed.Infer.TypeMismatch {
          type_provided = Typed.Type.Simple "Str";
          type_expected = Typed.Type.Simple "Float";
        }
      ]
      ~expect: []

  let test_inferred_dobule_nested_apply_mismatch () = test 
      ~node: Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [
          Typed.Type.Simple "Str"; 
          Typed.Type.Tuple [
            Typed.Type.Simple "Int";
            Typed.Type.Tuple [
              Typed.Type.Simple "Int";
              Typed.Type.Simple "Float";
            ]
          ];
          Typed.Type.Simple "Int"
        ];
        args = [
          local_ident_expr "hello" @@ Typed.Type.Var "t0";
          tuple_expr [
            local_ident_expr "hello" @@ Typed.Type.Simple "Int";
            tuple_expr [
              local_ident_expr "hello" @@ Typed.Type.Simple "Int";
              local_ident_expr "hello" @@ Typed.Type.Var "t0";
            ]
          ]
        ]
      }
      ~expect_type: (Typed.Type.Simple "Int")
      ~errors: [
        Typed.Infer.TypeMismatch {
          type_provided = Typed.Type.Simple "Str";
          type_expected = Typed.Type.Simple "Float";
        }
      ]
      ~expect: []

  let test_not_a_function () = test 
      ~node: Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda [
          Typed.Type.Simple "Str"; 
          Typed.Type.Simple "Str"; 
        ];
        args = [
          local_ident_expr "hello" @@ Typed.Type.Simple "Str";
          local_ident_expr "hello" @@ Typed.Type.Simple "Str";
          local_ident_expr "hello" @@ Typed.Type.Simple "Int";
        ]
      }
      ~expect_type: (Typed.Type.Simple "Str")
      ~errors: [
        Typed.Infer.NotFunction {
          type_provided = Typed.Type.Simple "Str";
        }
      ]
      ~expect: []

  let test_simple_generic () = test 
      ~node: Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda ~constr:["t"] [
          Typed.Type.Var "t"; 
          Typed.Type.Var "t"; 
        ];
        args = [
          local_ident_expr "hello" @@ Typed.Type.Simple "Str";
        ]
      }
      ~expect_type: (Typed.Type.Simple "Str")
      ~errors: []
      ~expect: []

  let test_simple_generic_multiparam () = test 
      ~node: Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda ~constr:["t"; "u"] [
          Typed.Type.Var "t"; 
          Typed.Type.Var "u"; 
          Typed.Type.Tuple [Typed.Type.Var "t"; Typed.Type.Var "u"]; 
        ];
        args = [
          local_ident_expr "str" @@ Typed.Type.Simple "Str";
          local_ident_expr "int" @@ Typed.Type.Simple "Int";
        ]
      }
      ~expect_type: (
          Typed.Type.Tuple [
            Typed.Type.Simple "Str";
            Typed.Type.Simple "Int"
          ] 
        )
      ~errors: []
      ~expect: []

  let test_simple_generic_mismatch () = test 
      ~node: Typed.Apply.{
        fn = global_scheme_expr "test" @@ Typed.Type.lambda ~constr:["t"] [
          Typed.Type.Var "t"; 
          Typed.Type.Var "t"; 
          Typed.Type.Var "t"; 
        ];
        args = [
          local_ident_expr "str" @@ Typed.Type.Simple "Str";
          local_ident_expr "int" @@ Typed.Type.Simple "Int";
        ]
      }
      ~expect_type: (Typed.Type.Simple "Str")
      ~errors: [
        Typed.Infer.TypeMismatch {
          type_provided = Typed.Type.Simple "Int";
          type_expected = Typed.Type.Simple "Str";
        }
      ]
      ~expect: []

  let test_lambda_infer () = test 
      ~node: Typed.Apply.{
        fn = global_ident_expr "test" @@ Typed.Type.Var "a0";
        args = [
          local_ident_expr "str" @@ Typed.Type.Simple "Str";
          local_ident_expr "int" @@ Typed.Type.Simple "Int";
        ]
      }
      ~expect_type: (
        (Typed.Type.lambda [
          Typed.Type.Var "t0"
        ]).typ
      )
      ~errors: []
      ~expect: [
        "a0", (Typed.Type.lambda [
          Typed.Type.Simple "Str";
          Typed.Type.Simple "Int";
          Typed.Type.Var "t0"
        ]).typ

      ]

  let test_func_one_arg () = test 
      ~node: Typed.Apply.{
        fn = global_ident_expr "int_to_int" @@ (Typed.Type.lambda [
          Typed.Type.Simple "Int";
          Typed.Type.Simple "Int";
        ]).typ;
        args = [
          Typed.Expr.Apply (Typed.Apply.{
            fn = global_ident_expr "fn" @@ Typed.Type.Var "a0";
            args = [
              local_ident_expr "int" @@ Typed.Type.Simple "Int";
            ];
          })
        ]
      }
      ~expect_type: (
        Typed.Type.Simple "Int";
      )
      ~errors: []
      ~expect: [
        "a0", (Typed.Type.lambda [
          Typed.Type.Simple "Int";
          Typed.Type.Simple "Int"
        ]).typ
      ]

  let test_func_arg_mismatch () = test 
      ~node: Typed.Apply.{
        fn = global_ident_expr "+" @@ (Typed.Type.lambda [
          Typed.Type.Simple "Int";
          Typed.Type.Simple "Int";
          Typed.Type.Simple "Int";
        ]).typ;
        args = [
          Typed.Expr.Apply (Typed.Apply.{
            fn = global_ident_expr "fn" @@ Typed.Type.Var "a0";
            args = [
              local_ident_expr "int" @@ Typed.Type.Simple "Int";
            ];
          });
          Typed.Expr.Apply (Typed.Apply.{
            fn = global_ident_expr "fn" @@ Typed.Type.Var "a0";
            args = [
              local_ident_expr "str" @@ Typed.Type.Simple "Str";
            ];
          })
        ]
      }
      ~expect_type: (
        Typed.Type.Simple "Int";
      )
      ~errors: [
        Typed.Infer.TypeMismatch {
          type_provided = Typed.Type.Simple "Str";
          type_expected = Typed.Type.Simple "Int";
        }
      ]
      ~expect: [
        "a0", (Typed.Type.lambda [
          Typed.Type.Simple "Int";
          Typed.Type.Simple "Int"
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

  let run_test () = Alcotest.run "Infer" [
    "InferTest", tests
  ]
end

let () = Infer.run_test ()