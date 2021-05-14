open Base
open Common
open Ast_test__helpers
open Ast.Node

let test = 
    test (Ast.Parser.Param.non_positional ~expr: Ast.Parser.expr) 
        (fun block -> Pp.branch [] (List.map block ~f:Param.pretty_print))


let tests = [
    (* TODO: why are () required *)
    "named", `Quick, (fun () -> test 
        ~input: "&foo" 
        ~expect: [
            Param.Named {name = Span.empty "foo"; shape = None}
        ]
    );

    (* TODO: type shouldn't eat the next parameter*)
    "named_destruct_name", `Quick, (fun () -> test 
        ~input: "&foo: f" 
        ~expect: [
            Param.Named {
                name = Span.empty "foo";
                shape = Some (Destruct.Name (Span.empty "f"))
            }
        ]
    );

    "named_parens", `Quick, (fun () -> test 
        ~input: "&(foo)" 
        ~expect: [
            Param.Named {name = Span.empty "foo"; shape = None};
        ]
    );

    "named_parens_optional", `Quick, (fun () -> test 
        ~input: "&(foo?)" 
        ~expect: [
            Param.Optional {name = Span.empty "foo"; alias = None; default = None};
        ]
    );

    "named_parens_optional_default", `Quick, (fun () -> test 
        ~input: "&(foo? = 2)" 
        ~expect: [
            Param.Optional {
                name = Span.empty "foo"; 
                alias = None; 
                default = Some (
                    Expr.Value(Value.Int(Span.empty "2"))
                )
            };
        ]
    );

    "named_group", `Quick, (fun () -> test 
        ~input: "&{foo; bar}" 
        ~expect: [
            Param.Named {name = Span.empty "foo"; shape = None};
            Param.Named {name = Span.empty "bar"; shape = None}
        ]
    );

    "named_group_newline", `Quick, (fun () -> test 
        ~input: "&{
            foo
            bar
        }" 
        ~expect: [
            Param.Named {name = Span.empty "foo"; shape = None};
            Param.Named {name = Span.empty "bar"; shape = None}
        ]
    );

    "named_group_shape", `Quick, (fun () -> test 
        ~input: "&{
            foo: f
            bar
        }" 
        ~expect: [
            Param.Named {name = Span.empty "foo"; shape = Some(Destruct.Name (Span.empty "f"))};
            Param.Named {name = Span.empty "bar"; shape = None}
        ]
    );

    "named_optional", `Quick, (fun () -> test 
        ~input: "&{foo?}" 
        ~expect: [
            Param.Optional {name = Span.empty "foo"; alias = None; default = None};
        ]
    );

    "optional_alias", `Quick, (fun () -> test 
        ~input: "&{foo?:f}" 
        ~expect: [
            Param.Optional {name = Span.empty "foo"; alias = Some(Span.empty "f"); default = None};
        ]
    );

    "named_default", `Quick, (fun () -> test 
        ~input: "&{foo? = 2 + 2}" 
        ~expect: [
            Param.Optional {name = Span.empty "foo"; alias = None; 
            default = Some(
                Expr.Apply(Apply.{
                    fn = Expr.Value(Value.Ident(Span.empty "+"));
                    args = [
                        Apply.PosArg{expr = Expr.Value(Value.Int(Span.empty "2"))};
                        Apply.PosArg{expr = Expr.Value(Value.Int(Span.empty "2"))};
                    ];
                    range = Span.empty_range
                }))
            };
        ]
    );

    "named_typed_destruct", `Quick, (fun () -> test 
        ~input: "&{foo?: f = 2 + 2}" 
        ~expect: [
            Param.Optional {name = Span.empty "foo"; alias = Some(Span.empty "f"); 
            default = Some(
                Expr.Apply(Apply.{
                    fn = Expr.Value(Value.Ident(Span.empty "+"));
                    args = [
                        Apply.PosArg{expr = Expr.Value(Value.Int(Span.empty "2"))};
                        Apply.PosArg{expr = Expr.Value(Value.Int(Span.empty "2"))};
                    ];
                    range = Span.empty_range
                }))
            };
        ]
    )
]
