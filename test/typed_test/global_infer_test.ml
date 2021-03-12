open Base
open Typed
open Common

let module_of code = 
    match Ast.of_string ~file:"" code with
    | Error e -> Alcotest.fail (Common.Err.to_string e); (* TODO: proper error string *)
    | Ok stmts ->
        let resolver = Typed.Resolver.Scope.root "" in
        Typed.Global.root ~resolve_source: (fun _ -> raise Common.TODO) "" resolver stmts

let split ~equals expect got =
    let expected_not_got = List.filter expect ~f: (fun e ->
        Option.is_none @@ List.find got ~f: (equals e)
    ) in
    let got_unexpected = List.filter got ~f: (fun g ->
        Option.is_none @@ List.find expect ~f: (equals g)
    ) in
    (expected_not_got, got_unexpected)

let test ?(expect_errors=[]) ~code ~expect = 
    match module_of code with
    | Ok (modu, _) -> (
        let lookup_scope_name modu scope_name =
            Option.value_exn (Typed.Node.Module.(modu.entries) 
            |> List.find_map ~f:(function
                | Typed.Node.Module.Binding b ->
                (*TODO: lookup_given_name *)
                    Common.log[b.given_name; scope_name];
                    if String.equal b.given_name scope_name then Some (Option.value_exn b.scheme) else None
                | _ -> None
            ))
        in
        let mismatch = List.filter_map expect ~f: (fun (scope_name, expect_scheme) ->
            let got_scheme = lookup_scope_name modu scope_name in
            if not @@ Typed.Type.scheme_equals expect_scheme got_scheme then (
                Some (scope_name, expect_scheme, got_scheme)
            ) else None
        ) in
        match mismatch with
        | [] -> ()
        | mismatches ->
            List.map mismatches ~f: (fun (scope_name, expect_scheme, got_scheme) ->
                String.concat [
                    "binding "; scope_name; ": ";
                    "expected:";
                    Typed.Type.scheme_to_string expect_scheme;
                    "!= got:";
                    Type.scheme_to_string got_scheme
                ]
            ) 
            |> String.concat ~sep: "\n"
            |> Alcotest.fail
        )
    | Error got_errors -> (
        let got_errors = List.map got_errors ~f: (Typed.Erro.clear_range) in
        let (missing_expected, unexpected) = split ~equals: (Typed.Erro.equals) expect_errors got_errors in
        if List.length missing_expected > 0 || List.length unexpected > 0 then begin 
            let missing_expected_str = List.map missing_expected ~f: (Typed.Erro.to_string) in
            let unexpected_str = List.map unexpected ~f: (Typed.Erro.to_string) in
            let missing_expected_block = match missing_expected_str with 
            | [] -> ""
            | errs -> "Missing expected:\n" ^ (String.concat ~sep:"\n" errs) ^ "\n"
            in
            let unexpected_block = match unexpected_str with 
            | [] -> ""
            | errs -> "Unexpected:\n" ^ (String.concat ~sep:"\n" errs) ^ "\n"
            in
            Alcotest.fail (missing_expected_block ^ unexpected_block)
        end;
    )

let test_simple_infer () =
    test 
    ~expect_errors: []
    ~code: {|
        let a = 10
        let b = a
    |} 
    ~expect: [
        "a", Type.make_scheme [] Typed.Base_types.int;
        "b", Type.make_scheme [] Typed.Base_types.int;
    ] 

let test_simple_generics () =
    test
    ~expect_errors: []
    ~code: {|
        let a x = x
        let b = a 10
    |} 
    ~expect: [
        (* TODO: It should be p0 or even some kind of t *)
        "a", Type.lambda ~constr:["p1"] [Type.Var "p1"; Type.Var "p1"];
        "b", Type.make_scheme [] Typed.Base_types.int;
    ] 

let test_pipe_operator () =
    test
    ~expect_errors: []
    ~code: {|
        let (|>) x f = f x
        let a x = x
        let b = 10 |> a 
    |}
    ~expect: [
        "a", Type.lambda ~constr:["p1"] [Type.Var "p1"; Type.Var "p1"];
        "b", Type.make_scheme [] Typed.Base_types.int;
    ] 

(* TODO: proper recursion checks *)
(* let test_rec_error () =
    test
    ~expect_errors: []
    ~code: {|
        let rec a = a
    |}
    ~expect: [
        "a", Type.lambda ~constr:["r0"] [Type.Var "r0"; Type.Var "r0"];
    ] 

let test_rec_complex_error () =
    test
    ~expect_errors: []
    ~code: {|
        let rec a = {
            let b = \() {a}
            b ()
        }
    |}
    ~expect: [
        "a", Type.lambda ~constr:["t0"] [Type.Var "t0"; Type.Var "t0"];
    ]  *)

let test_empty_list () =
    test
    ~expect_errors: []
    ~code: {|
        let a = []
    |}
    ~expect: [
        "a", Type.make_scheme [] (Typed.Type.Simple ("List", [Typed.Type.Var "t0"]));
    ] 

let test_single_item_list () =
    test
    ~expect_errors: []
    ~code: {|
        let a = ["1"]
    |}
    ~expect: [
        "a", Type.make_scheme [] (Typed.Type.Simple ("List", [Typed.Base_types.str]));
    ] 

let test_multiple_item_list () =
    test
    ~expect_errors: []
    ~code: {|
        let a = [1; 2; 3]
    |}
    ~expect: [
        "a", Type.make_scheme [] (Typed.Type.Simple ("List", [Typed.Base_types.int]));
    ] 

let test_type_mismatch_test() =
    test
    ~expect_errors: [
        Typed.Erro.ListItemTypeMismatch {
            range = Span.empty_range;
            unexpected = Base_types.str;
            expected = Base_types.int
        }
    ]
    ~code: {|
        let a = [1; "invalid"]
    |}
    ~expect: [
        "a", Type.make_scheme [] (Typed.Type.Simple ("List", [Typed.Base_types.int]));
    ] 

let tests = [
    "temp_simple_infer", `Quick, test_simple_infer;
    "test_simple_generics", `Quick, test_simple_generics;
    "test_pipe_operator", `Quick, test_pipe_operator;
    "test_empty_list", `Quick, test_empty_list;
    "test_single_item_list", `Quick, test_single_item_list;
    "test_multiple_item_list", `Quick, test_multiple_item_list;
    "test_type_mismatch_test", `Quick, test_type_mismatch_test;
    (* "test_rec_error", `Quick, test_rec_error;
    "test_rec_complex_error", `Quick, test_rec_complex_error *)
]