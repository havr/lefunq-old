open Base
open Common
open Typed
open Typed_infer_helper

module Coverage_test = struct 
    let check_cases = function
        | [] -> (raise Common.Unreachable)
        | h :: t ->
            let open Typed.Infer_match.Coverage in
            let h' = invert h in
            List.fold t ~init:h' ~f:(fun unc c ->
                let c' = invert c in
                let unc' = merge unc c' in
                unc'
            )

    let should_match name cases = (name, `Quick, (fun () ->
        let open Typed.Infer_match.Coverage in
        match check_cases cases with
        | [] -> ()
        | t -> Alcotest.fail ("expected exhaustive match, got:" ^ (list_to_string t))
    ))

    open Typed.Infer_match.Coverage
    let mkvar set n = Variant (n, set)
    let t = mkvar ["A"; "B"; "C"]
    let b = mkvar ["T"; "F"]

    let tests = [
        should_match "duplicate" [
            Tuple [b "T"; All];
            Tuple [b "T"; All];
        ]
    ]
end

(* let tests = Coverage_test.tests *)
(* let tests = Coverage_test.tests ["foo", `Quick, (fun () ->
    let open Typed.Infer_match.Coverage in
    (* let bools = ["T"; "F"] in *)
    (* let boo = mkvar bools in *)
    (* let mycov = Tuple [Uncountable (true, "0"); Variant ("T", bools)] in  *)

    (* "F".. -> "T"..None; *..None *)
    (*
    | [] -> ()
    | [T] -> ()
    | [_ .._] -> ()
    | t, C ->
    | t .. ->

    The following are unmatched: [F]
     *)
    check_cases [
        (* List ([], false); *)
        List ([All; t "C"], true);
        List ([t "A"; t "C"], true);
        List ([All; t "C"], true);
        (* List ([All; t "A"; t "A"], true);
        List ([All; All; t "A"], false);
        List ([All; All; All], true); *)
        (* List ([All; t "A"], true);
        List ([t "C"], false); *)

        (* Tuple [t "A"; t "B"];
        Tuple [t "A"; t "C"];
        Tuple [t "A"; t "A"];
        Tuple [t "B"; All];
        Tuple [t "C"; All]; *)

        (* Tuple [t "A"; t "B"];
        Tuple [t "B"; t "A"] *)
        (* Tuple [Uncountable (true, "0"); Variant (["T"], bools)];
        Tuple [Uncountable (true, "0"); Variant (["F"], bools)]; *)
        (* Tuple [Uncountable (true, "0"); All]; *)
        (* Tuple [All; Uncountable (true, "0")] *)
    ];
    Alcotest.fail("BOO")
    (* inv_perm mycov |> List.map ~f: to_string |> String.concat ~sep: "; " |> Alcotest.fail *)
)] *)

(* let tests = Coverage_test.tests *)
let tests = [
    (* TODO: move to let *)
    "let-fn: param", `Quick, test 
        ~code: {|
            let main n = n 
        |}
        ~expect: (Success {
            asserts = [
                assert_let "main" (Type.lambda ~constr: ["p0"] [Type.Var "p0"; Type.Var "p0"])
            ]
        });

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
                assert_ident "main.n" (Base_types.int);
                assert_ident "main.m" (Base_types.int)
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
                assert_ident "main.n" (Base_types.list Base_types.int);
                assert_ident "main.m" (Base_types.int);
                assert_ident "main.p" (Base_types.int)
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

    "tuples ok", `Quick, test 
        ~code: {|
            let main n = {
                n ? {
                    | a, 0 -> ()
                    | 0, b -> ()
                    | c, d -> ()
                }
            }
        |}
        ~expect: (Success {
            asserts = [
                assert_ident "main.n" (Type.Tuple [Base_types.int; Base_types.int]);
                assert_ident "main.a" (Base_types.int);
                assert_ident "main.b" (Base_types.int);
                assert_ident "main.c" (Base_types.int);
                assert_ident "main.d" (Base_types.int);
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
                    | _ -> ()
                }
            }
        |}
        ~expect: (Success {
            asserts = [
                assert_ident "main.t" (Base_types.int);
                assert_ident "main.u" (Base_types.int)
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
                assert_ident "main.n" (Base_types.list (Base_types.int));
                assert_ident "main.m" (Base_types.int);
                assert_ident "main.r" (Base_types.list (Base_types.int))
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
                assert_ident"main.a" (Base_types.int);
                assert_ident"main.t" (Type.Tuple [Base_types.int; Base_types.int]);
                assert_ident"main" (Base_types.int);
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
                assert_ident "main.a" (Base_types.int);
                assert_ident "main.b" (Base_types.str);
                assert_ident "main.c" (Base_types.int);
                assert_ident "main.item" (
                    Type.Tuple [Base_types.int; Base_types.list (Type.Tuple [Base_types.str; Base_types.int])]
                );
                assert_ident "main.rest" (
                    Base_types.list @@ Type.Tuple [Base_types.int; Base_types.list (Type.Tuple [Base_types.str; Base_types.int])]);
                assert_ident "main" (Base_types.unit) 
            ]
        })
]