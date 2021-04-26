open Common
open Ast_test__helpers
open Ast.Node

let test_case = test (Ast.Parser.Typedef.typedef) Typedef.pretty_print

let tests = [
    "foreign", `Quick, (fun () -> test_case
        ~input: "type Type = foreign"
        ~expect: (Typedef.{
            name = Span.empty "Type";
            params = [];
            def = Typedef.Foreign (Span.empty_range)
        })
    );

    "foreign_params", `Quick, (fun () -> test_case
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
