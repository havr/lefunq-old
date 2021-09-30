open Common
open Ast_test__helpers
open Ast.Node

let old_test_case = test (Ast.Parser.Typedef.typedef) Typedef.pretty_print
let new_test_case = test (Ast.Parser.typespace_name_decl) Module.pp_entry

let old_tests = [
    "foreign", `Quick, (fun () -> old_test_case
        ~input: "type Type = foreign"
        ~expect: (Typedef.{
            name = Span.empty "Type";
            params = [];
            def = Typedef.Foreign (Span.empty_range)
        })
    );

    "foreign_params", `Quick, (fun () -> old_test_case
        ~input: "type Type a b = foreign"
        ~expect: (Typedef.{
            name = Span.empty "Type";
            params = [
                {var = Span.empty "a"};
                {var = Span.empty "b"}
            ];
            def = Typedef.Foreign (Span.empty_range)
        })
    );
]

let new_tests = [
    "foreign", `Quick, (fun () -> new_test_case
        ~input: "Type = foreign"
        ~expect: (Module.Typedef (Typedef.{
            name = Span.empty "Type";
            params = [];
            def = Typedef.Foreign (Span.empty_range)
        }))
    );

    (* TODO *)
    (* "foreign_params", `Quick, (fun () -> test_case
        ~input: "type Type a b = foreign"
        ~expect: (Typedef.{
            name = Span.empty "Type";
            params = [
                {var = Span.empty "a"};
                {var = Span.empty "b"}
            ];
            def = Typedef.Foreign (Span.empty_range)
        })
    ); *)
]

let tests = new_tests @ old_tests;