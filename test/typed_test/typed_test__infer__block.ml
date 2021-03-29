open Base
open Typed_test__infer__helper

    let test ?(errors=[]) ~stmts ~expect_type ~expect = 
      let ctx = Typed.Inferno.{
        tempvar = Typed.Type_util.make_tempvar_gen "t";
        errors = [];
        substs = Map.empty(module String);
        env = Map.empty(module String);
      } in
      let typ = Typed.Infer.block ~ctx (Typed.Block.{stmts = stmts; range = Common.Span.empty_range}) in
      check_results ~ctx ~errors ~expect_type ~expect ctx.substs typ
      
    let test_returns_last_statement () = test
      ~stmts: [
        Typed.Stmt.Expr (Typed.Expr.Tuple (Typed.Tuple.{exprs=[]; range = Common.Span.empty_range}));
        Typed.Stmt.Expr (Typed.Expr.Value (Typed.Value.{value="10"; type_=Typed.Base_types.int; range = Common.Span.empty_range}))
      ]
      ~expect_type: (
        Typed.Base_types.int;
      )
      ~errors: []
      ~expect: []

    let test_not_unit_result_error () = test
      ~stmts: [
        Typed.Stmt.Expr (Typed.Expr.Value (Typed.Value.{value="hello"; type_=Typed.Base_types.str; range = Common.Span.empty_range}));
        Typed.Stmt.Expr (Typed.Expr.Value (Typed.Value.{value="10"; type_=Typed.Base_types.int; range = Common.Span.empty_range}))
      ]
      ~expect_type: (
        Typed.Base_types.int;
      )
      ~errors: [
        Typed.Erro.IgnoredResult {
          range = Common.Span.empty_range;
          unexpected = Typed.Base_types.str;
        }
      ]
      ~expect: []

    let tests = [
     (* TODO: fix and uncomment *)
      (* "ignored result", `Quick, test_not_unit_result_error; *)
      "last statement", `Quick, test_returns_last_statement
    ]