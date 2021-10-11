open Ast_test__helpers

open Common
open Ast.Node

let test_case = test (Ast.Parser.func_decl ()) Func.pp

let _tuple_value = "((), b: Str, (c: Int, d: Float))"
let _tuple_shape = FuncParam.Tuple [
    FuncParam.Unit;
    FuncParam.Name {
        name = Span.empty "b";
        typ = Type.Simple{name = Span.empty "Str"; args = []}
    };
    FuncParam.Tuple [
        FuncParam.Name {
            name = Span.empty "c";
            typ = Type.Simple{name = Span.empty "Int"; args = []}
        };
        FuncParam.Name {
            name = Span.empty "d";
            typ = Type.Simple{name = Span.empty "Float"; args = []}
        };
    ]
]

let tests = [
    "positional_unit", `Quick, (fun () -> test_case 
        ~input: "test | () = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Positional (FuncParam.Unit)
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "positional_name_unit", `Quick, (fun () -> test_case 
        ~input: "test | param: () = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Positional (FuncParam.Name FuncParam.{
                    name = Span.empty "param";
                    typ = Type.Unit
                })
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "positional_name_type", `Quick, (fun () -> test_case 
        ~input: "test | param: Int = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Positional (FuncParam.Name FuncParam.{
                    name = Span.empty "param";
                    typ = Type.Simple{name = Span.empty "Int"; args = []}
                })
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "positional_name_type_multiple_blocks", `Quick, (fun () -> test_case 
        ~input: "test | param: Int | another: Str = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Positional (FuncParam.Name FuncParam.{
                    name = Span.empty "param";
                    typ = Type.Simple{name = Span.empty "Int"; args = []}
                });
                FuncParam.Positional (FuncParam.Name FuncParam.{
                    name = Span.empty "another";
                    typ = Type.Simple{name = Span.empty "Str"; args = []}
                })
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "positional_name_type_multiple", `Quick, (fun () -> test_case 
        ~input: "test | param: Int another: Str = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Positional (FuncParam.Name FuncParam.{
                    name = Span.empty "param";
                    typ = Type.Simple{name = Span.empty "Int"; args = []}
                });
                FuncParam.Positional (FuncParam.Name FuncParam.{
                    name = Span.empty "another";
                    typ = Type.Simple{name = Span.empty "Str"; args = []}
                })
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "named_typed", `Quick, (fun () -> test_case 
        ~input: "test | &named: Int = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Named {
                    name = Span.empty "named";
                    kind = Typed (Type.Simple{name = Span.empty "Int"; args = []})
                }
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "named_typed_muliple", `Quick, (fun () -> test_case 
        ~input: "test | &named: Int &another: Str = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Named {
                    name = Span.empty "named";
                    kind = Typed (Type.Simple{name = Span.empty "Int"; args = []})
                };
                FuncParam.Named {
                    name = Span.empty "another";
                    kind = Typed (Type.Simple{name = Span.empty "Str"; args = []})
                }
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "named_shaped", `Quick, (fun () -> test_case 
        ~input: ("test | &named: " ^ (_tuple_value) ^ " = ()")
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Named {
                    name = Span.empty "named";
                    kind = Shaped (_tuple_shape)
                }
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "optional_typed", `Quick, (fun () -> test_case 
        ~input: "test | ?named: Int = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Optional {
                    name = Span.empty "named";
                    kind = FuncParam.WithType (Type.Simple{name = Span.empty "Int"; args = []})
                }
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "optional_default", `Quick, (fun () -> test_case 
        ~input: "test | ?named: (rename = 42) = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Optional {
                    name = Span.empty "named";
                    kind = FuncParam.WithDefault {
                        name = Span.empty "rename";
                        expr = Expr.Value (Value.Int (Span.empty "42"))
                    }
                }
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "positional_shape_single_name", `Quick, (fun () -> test_case 
        ~input: "test | (param: Int) = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Positional (FuncParam.Name FuncParam.{
                    name = Span.empty "param";
                    typ = Type.Simple{name = Span.empty "Int"; args = []}
                })
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "positional_shape_tuple", `Quick, (fun () -> test_case 
        ~input: ("test | " ^ (_tuple_value) ^ " = ()")
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Positional (
                    _tuple_shape
                );
            ];
            return = None;
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    );

    "return", `Quick, (fun () -> test_case 
        ~input: "test | param: Str |: Int = ()"
        ~expect: Func.{
            range = Span.empty_range;
            name = Span.empty "test";
            params = [
                FuncParam.Positional (FuncParam.Name FuncParam.{
                    name = Span.empty "param";
                    typ = Type.Simple{name = Span.empty "Str"; args = []}
                })
            ];
            return = Some (Type.Simple{name = Span.empty "Int"; args = []});
            expr = Func.Expr (Expr.Value (Value.Unit (Span.empty ())));
            is_rec = false 
        }
    )
]