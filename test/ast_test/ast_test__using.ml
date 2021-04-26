open Common
open Ast_test__helpers
open Ast.Node

let test_case = test (Ast.Parser.using) Using.pretty_print

let tests = [
    "source as module", `Quick, (fun () -> test_case
        ~input: {|using "source" as Module |}
        ~expect: (Using.{
            keyword = Span.empty "Type";
            kind = Using.Global (Span.empty "source");
            action = Using.Rename (Span.empty "Module");
        })
    );
]
