open Typed
open Base
open Helper
open Typed_test__infer__helper

let test ?(errors=[]) ~stmts ~expect_type ~expect = 
    let ctx = Infer.{
        tempvar = Type_util.make_tempvar_gen "t";
        errors = [];
        substs = Map.empty(module String);
        env = Map.empty(module String);
    } in 
        let typ = Typed.Infer.block ~ctx (Typed.Block.{stmts = stmts; range = Common.Span.empty_range}) in
        check_results ~ctx ~errors ~expect_type ~expect ctx.substs typ


let let_single () = test
    ~stmts: [
        let_stmt "hello" [
            value_stmt "10" (Base_types.int)
        ];
        local_ident_stmt "hello" 
    ]
    ~expect_type: (
        simple Base_types.int_name;
    )
    ~errors: []
    ~expect: []
    

let let_lambda () = test
    ~stmts: [
        let_stmt "hello" [
            lambda_stmt [Param.{given = "a"; resolved= ""}, Type.Var "t"] [
            local_ident_stmt ~scheme:(Type.make_scheme [] (Type.Var "t")) "a"
            ]
        ];
        local_ident_stmt "hello"
    ]
    ~expect_type: (
        (Type.lambda [Type.Var "t0"; Type.Var "t0"]).typ;
    )
    ~errors: []
    ~expect: []

let tests = [
    "let:single", `Quick, let_single;
    "let:lambda", `Quick, let_lambda;
]