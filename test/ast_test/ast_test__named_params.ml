open Base
open Common
open Ast_test__helpers
open Ast.Node

let test = 
    test (Ast.Parser.Param.named ~expr: Ast.Parser.expr) 
        (fun block -> Pp.branch [] (List.map block ~f:Param.pretty_print))


let tests = [
    (* TODO: why are () required *)
    "named", `Quick, (fun () -> test 
        ~input: "&foo" 
        ~expect: [
            Param.Named {name = Span.empty "foo"; typ = None}
        ]
    );

    (* TODO: type shouldn't eat the next parameter*)
    "named_typed", `Quick, (fun () -> test 
        ~input: "&foo: Int" 
        ~expect: [
            Param.Named {name = Span.empty "foo"; typ = Some(Type.Simple {name = Span.empty "Int"; args = []})}
        ]
    );

    "named_parens", `Quick, (fun () -> test 
        ~input: "&(foo)" 
        ~expect: [
            Param.Named {name = Span.empty "foo"; typ = None};
        ]
    );

    "named_parens_optional", `Quick, (fun () -> test 
        ~input: "&(foo?)" 
        ~expect: [
            Param.Optional {name = Span.empty "foo"; typ = None; expr = None};
        ]
    );

    "named_parens_optional_default", `Quick, (fun () -> test 
        ~input: "&(foo = 2)" 
        ~expect: [
            Param.Optional {
                name = Span.empty "foo"; 
                typ = None; 
                expr = Some (
                    Expr.Value(Value.Int(Span.empty "2"))
                )
            };
        ]
    );

    "named_parens_optional_default_alt", `Quick, (fun () -> test 
        ~input: "&(foo? = 2)" 
        ~expect: [
            Param.Optional {
                name = Span.empty "foo"; 
                typ = None; 
                expr = Some (
                    Expr.Value(Value.Int(Span.empty "2"))
                )
            };
        ]
    );

    "named_group", `Quick, (fun () -> test 
        ~input: "&{foo; bar}" 
        ~expect: [
            Param.Named {name = Span.empty "foo"; typ = None};
            Param.Named {name = Span.empty "bar"; typ = None}
        ]
    );

    "named_group_newline", `Quick, (fun () -> test 
        ~input: "&{
            foo
            bar
        }" 
        ~expect: [
            Param.Named {name = Span.empty "foo"; typ = None};
            Param.Named {name = Span.empty "bar"; typ = None}
        ]
    );

    "named_group_typed", `Quick, (fun () -> test 
        ~input: "&{
            foo: Int
            bar
        }" 
        ~expect: [
            Param.Named {name = Span.empty "foo"; typ = Some(Type.simple (Span.empty "Int") [])};
            Param.Named {name = Span.empty "bar"; typ = None}
        ]
    );

    "named_optional", `Quick, (fun () -> test 
        ~input: "&{foo?}" 
        ~expect: [
            Param.Optional {name = Span.empty "foo"; typ = None; expr = None};
        ]
    );

    "named_typed_optional", `Quick, (fun () -> test 
        ~input: "&{foo?:Int}" 
        ~expect: [
            Param.Optional {name = Span.empty "foo"; typ = Some(Type.simple (Span.empty "Int") []); expr = None};
        ]
    );

    "named_default", `Quick, (fun () -> test 
        ~input: "&{foo = 2 + 2}" 
        ~expect: [
            Param.Optional {name = Span.empty "foo"; typ = None; 
            expr = Some(
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

    "named_typed_default", `Quick, (fun () -> test 
        ~input: "&{foo: Int = 2 + 2}" 
        ~expect: [
            Param.Optional {name = Span.empty "foo"; typ = Some(Type.simple (Span.empty "Int") []); 
            expr = Some(
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
