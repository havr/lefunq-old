open Typed
open Base
open Common

let rec find_toplevel_let path modu_stmts =
    match path, modu_stmts with
    | [], _ -> None
    | name :: [], entries -> 
        (List.find_map entries ~f:(function
        | Typed.Module.Binding t when String.equal t.given_name name -> Some t 
        | _ -> None
        ) |> Option.map ~f: (fun t -> (t, [])))
    | name :: rest, entries -> (
        let rec iter = function 
            | (Typed.Module.Module m) :: _ when String.equal m.given_name name -> 
                find_toplevel_let rest m.entries
            | (Typed.Module.Binding b) :: _ when String.equal b.given_name name -> 
                Some (b, rest)
            | _ :: rest -> iter rest
            | [] -> None
        in iter entries
    )

let find_let_in_stmts path stmts =
    let rec find_in_stmts path stmts = List.find_map ~f: (find_in_stmt path) stmts
    and find_in_stmt path block =
        match (block, path) with
        | Typed.Stmt.Let t, n :: [] -> (match String.equal t.given_name n with
            | true -> Some t
            | false -> (List.find_map t.block.stmts ~f: (find_in_stmt path))
        )
        | Typed.Stmt.Let t, n :: rest -> (match String.equal t.given_name n with
            | true -> (List.find_map t.block.stmts ~f: (find_in_stmt rest))
            | false -> None
        )
        | Typed.Stmt.Let _, [] -> None 
        | Typed.Stmt.Block b, path -> find_in_stmts path (b.stmts)
        | Typed.Stmt.Expr e, path -> find_in_expr path e
    and find_in_expr path expr = 
        match expr with
        | Value _ -> None 
        | Ident _ -> None
        | Apply a -> (match find_in_expr path a.fn with
            | Some m -> Some m
            | None -> List.find_map a.args ~f: (find_in_expr path)
        )
        | Lambda lam ->
            find_in_stmts path lam.block.stmts
        | Cond c ->
            (match (List.find_map c.cases ~f: (fun case -> 
                match find_in_stmts path case.if_.stmts with
                | Some m -> Some m
                | None -> find_in_stmts path case.then_.stmts
            )) with
            | Some m -> Some m
            | None -> (Option.find_map c.else_ ~f: (fun b -> find_in_stmts path b.stmts)))
        | Li li -> List.find_map li.items ~f:(find_in_expr path)
        | Tuple tup ->  List.find_map tup.exprs ~f:(find_in_expr path)
        | Foreign _ -> None
        | Match m -> (match find_in_expr path m.expr with
            | Some m -> Some m
            | None -> List.find_map m.cases ~f: (fun case -> find_in_stmts path case.stmts)
        )
    in find_in_stmts path stmts

let find_ident name stmts =
    let rec find_in_stmts = List.find_map ~f: (function
        | Typed.Stmt.Let _ -> None 
        | Typed.Stmt.Block b -> find_in_stmts (b.stmts)
        | Typed.Stmt.Expr e -> find_in_expr e
    )
    and find_in_pattern = function
        | Typed.Match.Param p when String.equal p.given_name name -> Some (Type.make_scheme [] p.typ)
        | Typed.Match.Tuple t -> List.find_map t ~f: find_in_pattern
        | Typed.Match.List li -> (match List.find_map li.items ~f: find_in_pattern with
            | Some m -> Some m
            | None -> Option.find_map li.rest ~f: find_in_pattern
        )
        | _ -> None

    and find_in_expr = function
        | Value _ -> None 
        | Ident t when String.equal t.resolved.given name -> Some (Option.value_exn t.scheme)
        | Ident _ -> None
        | Apply a -> (match find_in_expr a.fn with
            | Some m -> Some m
            | None -> List.find_map a.args ~f: find_in_expr
        )
        | Lambda lam ->
            find_in_stmts lam.block.stmts
        | Cond c ->
            (match (List.find_map c.cases ~f: (fun case -> 
                match find_in_stmts case.if_.stmts with
                | Some m -> Some m
                | None -> find_in_stmts case.then_.stmts
            )) with
            | Some m -> Some m
            | None -> (Option.find_map c.else_ ~f: (fun b -> find_in_stmts b.stmts)))
        | Li li -> List.find_map li.items ~f:find_in_expr
        | Tuple tup ->  List.find_map tup.exprs ~f:find_in_expr
        | Foreign _ -> None
        | Match m -> (match find_in_expr m.expr with
            | Some m -> Some m
            | None -> List.find_map m.cases ~f: (fun case -> 
                match find_in_pattern case.pattern with
                | Some m -> Some m
                | None -> find_in_stmts case.stmts
            )
        )
    in find_in_stmts stmts

let trunc_last list = match List.rev list with
    | [] -> raise (Invalid_argument "list is empty")
    | head :: rest -> head, List.rev rest

open Typed_test__infer__helper

let assert_ident_scheme str_path scheme stmts =
    let path = String.split ~on: '.' str_path in
    let result = (match find_toplevel_let path stmts with
    | None -> None
    | Some (b, []) -> b.scheme
    | Some (b, rest) -> 
        let name, rest' = trunc_last rest in
        (match rest' with
        | [] -> Some b
        | rest' -> find_let_in_stmts rest' b.block.stmts)
            |> Option.find_map ~f:(fun t -> find_ident name Let.(t.block.stmts))
    ) in
    (match result with
    | None -> Alcotest.fail ("ident " ^ str_path ^ " not found")
    | Some result_scheme -> Alcotest.(check(module String) 
        (Type.scheme_to_string scheme) 
        (Type.scheme_to_string result_scheme))) 
    |> ignore

type test_result = Success of {
    asserts: (Module.entry list -> unit) list
} | Failure of {
    errors: (Erro.t list)
}

let test ~code ~expect () = 
    match Ast.of_string ~file:"" code with
    | Error e -> Alcotest.fail (Common.Err.to_string e)
    | Ok ast -> 
        let result = root ~source: "" ~resolve_source: (fun _ -> raise Common.Unexpected) ast in 
        match (expect, result) with
        | (Success {asserts}, Ok ast) -> 
            List.iter asserts ~f: (fun asser -> asser ast.entries)
        | Failure {errors}, Error got_errors ->
            assert_errors errors (List.map ~f:Erro.clear_range got_errors)
        | Success _, Error e -> (Alcotest.fail (e |> List.map ~f:Typed.Erro.to_string |> String.concat ~sep:"\n"))
        | Failure _, Ok ast -> 
            Common.log ["\n"; Pp.to_string [Typed.Module.pretty_print ast]];
            Alcotest.fail ("Expected failure, got OK")
        
let tests = [
    "simple: ok", `Quick, test 
        ~code: {|
            let main n = {
                n ? {
                    | 0 -> ()
                    | m -> ()
                }
            }
        |}
        ~expect: (Success {
            asserts = [
                assert_ident_scheme "main.n" (Type.make_scheme [] Base_types.int);
                assert_ident_scheme "main.m" (Type.make_scheme [] Base_types.int)
            ]
        });

    "list unification: ok", `Quick, test 
        ~code: {|
            let main n = {
                n ? {
                    | [m; 0; p] -> p + m
                    | _ -> 0
                }
            }
        |}
        ~expect: (Success {
            asserts = [
                assert_ident_scheme "main.n" (Type.make_scheme [] Base_types.int);
                assert_ident_scheme "main.m" (Type.make_scheme [] Base_types.int);
                assert_ident_scheme "main.p" (Type.make_scheme [] Base_types.int)
            ]
        });

    "list unification: value mismatch", `Quick, test 
        ~code: {|
            let main n = {
                n ? {
                    | [m; "hello"; p] -> p + 1
                    | _ -> 0
                }
            }
        |}
        ~expect: (Failure {
            errors = [
                TypeMismatch {
                    range = Span.empty_range;
                    (* uniform scheme for such errors *)
                    type_expected = Base_types.int;
                    type_provided = Base_types.str;
                }
            ]
        });

    "duplicate patterns: simple", `Quick, test 
        ~code: {|
            let main n = {
                n ? {
                    | m -> ()
                    | m -> ()
                    | _ -> ()
                }
            }
        |}
        ~expect: (Failure {
            errors = [
                UnusedMatchCase {
                    range = Span.empty_range;
                }
            ]
        });

    "duplicate: nested", `Quick, test 
        ~code: {|
            let main n = {
                n ? {
                    | m, 0 -> ()
                    | _, 0 -> ()
                }
            }
        |}
        ~expect: (Failure {
            errors = [
                UnusedMatchCase {
                    range = Span.empty_range;
                };
                NonExhaustivePatternMatching {
                    range = Span.empty_range;
                    missing_cases = [];
                }
            ]
        });

    "non exhaustive: empty list", `Quick, test 
        ~code: {|
            let main n = {
                n ? {
                    | [] -> ()
                    | [_; _ ..rest] -> ()
                }
            }
        |}
        ~expect: (Failure {
            errors = [
                NonExhaustivePatternMatching {
                    range = Span.empty_range;
                    missing_cases = [];
                }
            ]
        });

    "duplicate patterns: lists", `Quick, test 
        ~code: {|
            let main n = {
                n ? {
                    | [1 ..rest] -> ()
                    | [1; t ..rest] -> ()
                    | _ -> ()
                }
            }
        |}
        ~expect: (Failure {
            errors = [
                UnusedMatchCase {
                    range = Span.empty_range;
                }
            ]
        });

    "tuples: ok", `Quick, test 
        ~code: {|
            let main n = {
                n ? {
                    | 0, t -> ()
                    | u, 1 -> ()
                }
            }
        |}
        ~expect: (Success {
            asserts = [
                assert_ident_scheme "main.t" (Type.make_scheme [] Base_types.int);
                assert_ident_scheme "main.u" (Type.make_scheme [] Base_types.int)
            ]
        });

    "lists: ok", `Quick, test 
        ~code: {|
            let main n = {
                n ? {
                    | [] -> ()
                    | [1] -> ()
                    | [1; m] -> ()
                    | [1; 2 ..r] -> ()
                    | _ -> ()
                }
            }
        |}
        ~expect: (Success {
            asserts = [
                assert_ident_scheme "main.n" (Type.make_scheme [] @@ Base_types.list (Base_types.int));
                assert_ident_scheme "main.m" (Type.make_scheme [] Base_types.int);
                assert_ident_scheme "main.r" (Type.make_scheme [] @@ Base_types.list (Base_types.int))
            ]
        });

    "tuple: ok", `Quick, test 
        ~code: {|
            let main = {
                (1, 2) ? {
                    | (a, 0) -> a
                    | t -> 0
                }
            }
        |}
        ~expect: (Success {
            asserts = [
                assert_ident_scheme "main.a" (Type.make_scheme [] (Base_types.int));
                assert_ident_scheme "main.t" (Type.make_scheme [] (Type.Tuple [Base_types.int; Base_types.int]));
                assert_ident_scheme "main" (Type.make_scheme [] (Type.Tuple [Base_types.int; Base_types.int]));
            ]
        });

    "nested: ok", `Quick, test 
        ~code: {|
            let main = {
                [1, ["hello", 0]] ? {
                    | [] -> ()
                    | [a, [b, c]] -> ()
                    | [item ..rest] -> ()
                }
            }
        |}
        ~expect: (Success {
            asserts = [
                assert_ident_scheme "main.a" (Type.make_scheme [] (Base_types.int));
                assert_ident_scheme "main.b" (Type.make_scheme [] (Base_types.str));
                assert_ident_scheme "main.c" (Type.make_scheme [] (Base_types.int));
                assert_ident_scheme "main.item" (Type.make_scheme [] (
                    Type.Tuple [Base_types.int; Type.Tuple [Base_types.str; Base_types.str]]
                ));
                assert_ident_scheme "main.rest" (Type.make_scheme [] (
                    Base_types.list @@ Type.Tuple [Base_types.int; Type.Tuple [Base_types.str; Base_types.str]]));
                assert_ident_scheme "main" (Type.make_scheme [] (
                    Base_types.list @@ Type.Tuple [Base_types.int; Type.Tuple [Base_types.str; Base_types.str]]));
            ]
        })
]