open Base
open Typed

let module_of code = 
    match Ast.of_string ~file:"" code with
    | Error _ -> Alcotest.fail ""; (* TODO: proper error string *)
    | Ok stmts -> begin 
        let resolver = Typed.Resolver.Global.root() in
        Typed.Global.root resolver stmts
    end

let split ~equals expect got =
    let expected_not_got = List.filter expect ~f: (fun e ->
        Option.is_none @@ List.find got ~f: (fun g -> equals e g)
    ) in
    let got_unexpected = List.filter got ~f: (fun g ->
        Option.is_none @@ List.find expect ~f: (fun e -> equals e g)
    ) in
    (expected_not_got, got_unexpected)

let test ?(expect_errors=[]) ~code ~expect = 
    let (modu, got_errors) = module_of code in
    let (missing_expected, unexpected) = split ~equals: (Typed.Error.equals) expect_errors got_errors in
    if List.length missing_expected > 0 || List.length unexpected > 0 then begin 
        let missing_expected_str = List.map missing_expected ~f: (Typed.Error.to_string) in
        let unexpected_str = List.map unexpected ~f: (Typed.Error.to_string) in
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
    let lookup_scope_name modu scope_name =
        Option.value_exn (Typed.Node.Module.(modu.entries) 
        |> List.find_map ~f:(function
            | Typed.Node.Module.Binding b ->
                 if String.equal b.scope_name scope_name then Some (Option.value_exn b.scheme) else None
        ))
    in
    let mismatch = List.filter_map expect ~f: (fun (scope_name, expect_scheme) ->
        let got_scheme = lookup_scope_name modu scope_name in
        if not @@ Typed.Type.scheme_equals expect_scheme got_scheme then begin 
            Some (scope_name, expect_scheme, got_scheme)
        end else None
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

let test_simple_infer () =
    test 
    ~expect_errors: []
    ~code: {|
        let a = 10
        let b = a
    |} 
    ~expect: [
        "a", Type.make_scheme [] Typed.BaseTypes.int;
        "b", Type.make_scheme [] Typed.BaseTypes.int;
    ] 

let test_simple_generics () =
    test
    ~expect_errors: []
    ~code: {|
        let a x = x
        let b = a 10
    |} 
    ~expect: [
        "a", Type.lambda ~constr:["p0"] [Type.Var "p0"; Type.Var "p0"];
        "b", Type.make_scheme [] Typed.BaseTypes.int;
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
        "a", Type.lambda ~constr:["p0"] [Type.Var "p0"; Type.Var "p0"];
        "b", Type.make_scheme [] Typed.BaseTypes.int;
    ] 

let test_rec_error () =
    test
    ~expect_errors: []
    ~code: {|
        let rec a = a
    |}
    ~expect: [
        "a", Type.lambda ~constr:["p0"] [Type.Var "p0"; Type.Var "p0"];
    ] 

let test_rec_infinite_error () =
    test
    ~expect_errors: []
    ~code: {|
        let rec a = {
            let b = \() {a}
            b ()
        }
    |}
    ~expect: [
        "a", Type.lambda ~constr:["p0"] [Type.Var "p0"; Type.Var "p0"];
    ] 

let tests = [
    (*"temp_simple_infer", `Quick, test_simple_infer;
    "test_simple_generics", `Quick, test_simple_generics;
    "test_pipe_operator", `Quick, test_pipe_operator*)
    (*"test_rec_error", `Quick, test_rec_error;*)
    "test_rec_error", `Quick, test_rec_infinite_error
]