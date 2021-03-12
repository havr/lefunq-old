open Common
open Helper
open Typed
open Base

let shadowing () =
    let input = [
        let_stmt "a" [value_stmt "10" (Base_types.int)];
        let_stmt "a" [value_stmt "str" (Base_types.str)]
    ] in
    let input_block = (Block.{stmts=input; range = Span.empty_range}) in
    let global = Resolver.Scope.root "" in
    let ctx = Resolve.make_context (Resolver.Scope.local_root global) in
    let resolved = Resolve.block ctx input_block in
    let expect = Block.{
        range = Span.empty_range;
        stmts = [
            let_stmt ~scope_name: "a" "a" [value_stmt "10" (Base_types.int)];
            let_stmt ~scope_name: "a$2" "a" [value_stmt "str" (Base_types.str)]
        ]
    } in
    if not @@ (Block.equals resolved input_block) then
        Alcotest.fail @@ String.concat ~sep: "\n" [
            "expected:";
            (Block.to_string expect);
            "got:";
            (Block.to_string resolved);
        ]

let tests = [
    "shadowing", `Quick, shadowing
]