open Common
open Ast_test__helpers
open Ast.Node

let block_test_case = test (Ast.Parser.pattern_block()) Match.pretty_print_cases
let matc_test_case = test (Ast.Parser.expr()) Expr.pretty_print

let matc_tests = [
    "tuple", `Quick, (fun () -> matc_test_case
        ~input: "(1, 2) ? { 
            | t -> ()
        }"
        ~expect: (Expr.Match (Match.{
            range = Span.empty_range;
            expr = Expr.Value(Value.Tuple(Tuple.{
                range = Span.empty_range;
                exprs = [
                    Expr.Value (Value.Int (Span.empty "1"));
                    Expr.Value (Value.Int (Span.empty "2"));
                ]
            }));
            block = Match.{
                range = Span.empty_range;
                cases = [
                    {
                        pattern = Param (Span.empty "t");
                        stmts = [Block.Expr (Expr.Value (Value.Unit (Span.empty ())))]
                    }
                ];
            }
        }))
    );
    "match", `Quick, (fun () -> matc_test_case
        ~input: "func hello ? { 
            | t -> 1
        }"
        ~expect: (Expr.Match (Match.{
            range = Span.empty_range;
            expr = Expr.Apply (Apply.{
                range = Span.empty_range;
                fn = Expr.Value (Value.Ident (Span.empty "func"));
                args = [
                    Apply.PosArg{expr = Expr.Value (Value.Ident(Span.empty "hello"))};
                ]
            });
            block = Match.{
                range = Span.empty_range;
                cases = [
                    {
                        pattern = Param (Span.empty "t");
                        stmts = [Block.Expr (Expr.Value (Value.Int (Span.empty "1")))]
                    }
                ];
            }
        }))
    )
]

let block_tests = [
    "lists", `Quick, (fun () -> block_test_case
        ~input: "{
            | [] -> ()
            | [1] -> ()
            | [1; 2] -> ()
            | [1; 2 ..n] -> ()
        }"
        ~expect: Match.{
            range = Span.empty_range;
            cases = [
                {
                    pattern = List {
                        items = [];
                        rest = None
                    };                     
                    stmts = [
                        Block.Expr (Expr.Value (Value.Unit (Span.empty ())));
                    ]
                };
                {
                    pattern = List {
                        items = [
                            Int (Span.empty "1");
                        ];
                        rest = None
                    };                     
                    stmts = [
                        Block.Expr (Expr.Value (Value.Unit (Span.empty ())));
                    ]
                };
                {
                    pattern = List {
                        items = [
                            Int (Span.empty "1");
                            Int (Span.empty "2");
                        ];
                        rest = None
                    };                     
                    stmts = [
                        Block.Expr (Expr.Value (Value.Unit (Span.empty ())));
                    ]
                };
                {
                    pattern = List {
                        items = [
                            Int (Span.empty "1");
                            Int (Span.empty "2");
                        ];
                        rest = (Some (Param (Span.empty "n")))
                    };                     
                    stmts = [
                        Block.Expr (Expr.Value (Value.Unit (Span.empty ())));
                    ]
                };
            ]
        }
    );
    "block", `Quick, (fun () -> block_test_case
        ~input: "{
            | a -> 1 
            | 1 -> 
                2
            | \"text\" -> 
                3
                4
            | 1, hello -> ()
            | (1, world) -> ()
            | [1, hello; 2, world .. rest] -> ()
        }"
        ~expect: Match.{
            range = Span.empty_range;
            cases = [
                {
                    pattern = Param (Span.empty "a");
                    stmts = [Block.Expr (Expr.Value (Value.Int (Span.empty "1")))]
                };
                {
                    pattern = Int (Span.empty "1");
                    stmts = [Block.Expr (Expr.Value (Value.Int (Span.empty "2")))]
                };
                {
                    pattern = Str (Span.empty "text");
                    stmts = [
                        Block.Expr (Expr.Value (Value.Int (Span.empty "3")));
                        Block.Expr (Expr.Value (Value.Int (Span.empty "4")))
                    ]
                };
                {
                    pattern = Tuple [
                        Int (Span.empty "1");
                        Param (Span.empty "hello");
                    ];
                    stmts = [
                        Block.Expr (Expr.Value (Value.Unit (Span.empty ())));
                    ]
                };
                {
                    pattern = Tuple [
                        Int (Span.empty "1");
                        Param (Span.empty "world");
                    ];
                    stmts = [
                        Block.Expr (Expr.Value (Value.Unit (Span.empty ())));
                    ]
                };
                {
                    pattern = List {
                        items = [
                            Tuple [
                                Int (Span.empty "1");
                                Param (Span.empty "hello");
                            ];
                            Tuple [
                                Int (Span.empty "2");
                                Param (Span.empty "world");
                            ];
                        ];
                        rest = Some (Param (Span.empty "rest"))
                    };                     
                    
                    stmts = [
                        Block.Expr (Expr.Value (Value.Unit (Span.empty ())));
                    ]
                }
            ]
        }
    );
]

let tests = matc_tests @ block_tests @ [];