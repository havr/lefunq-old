open Base
open Typed

let module_of code = 
    match Ast.of_string ~file:"" code with
    | Error _ -> Alcotest.fail ""; (* TODO: proper error string *)
    | Ok stmts -> begin 
        let typed = Typed.Global.root stmts in
        typed
    end

let test_simple_infer () =
    let code = {|
        let a = 10
        let b = a
    |} in
    let expect_bindings = [
        "a", Type.make_scheme [] Typed.BaseTypes.int;
        "b", Type.make_scheme [] Typed.BaseTypes.int;
    ] in
    let modu = module_of code in
    let lookup_scope_name modu scope_name =
        Option.value_exn (Typed.Node.Module.(modu.entries) 
        |> List.find_map ~f:(function
            | Typed.Node.Module.Binding b ->
                 if String.equal b.scope_name scope_name then Some (Option.value_exn b.scheme) else None
        ))
    in
    let mismatch = List.filter_map expect_bindings ~f: (fun (scope_name, expect_scheme) ->
        let got_scheme = lookup_scope_name modu scope_name in
        if not @@ Typed.Type.scheme_equals expect_scheme got_scheme then begin 
            Some (expect_scheme, got_scheme)
        end else None
    ) in
    match mismatch with
    | [] -> ()
    | mismatches ->
        List.map mismatches ~f: (fun (expect_scheme, got_scheme) ->
            String.concat [
                "expected:";
                Typed.Type.scheme_to_string expect_scheme;
                "!= got:";
                Type.scheme_to_string got_scheme
            ]
        ) 
        |> String.concat ~sep: "\n"
        |> Alcotest.fail



let tests = [
    "simple_infer", `Quick, test_simple_infer
]