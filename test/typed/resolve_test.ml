open Helper
open Typed
open Base

let shadowing () =
    let input = [
        let_stmt "a" [value_stmt "10" (BaseTypes.int)];
        let_stmt "a" [value_stmt "str" (BaseTypes.str)]
    ] in
    let input_block = (Block.{stmts=input}) in
    let global = Resolver.Global.root() in
    let ctx = Resolve.make_context (Resolver.Local.make global) in
    let resolved = Resolve.block ctx input_block in
    let expect = Block.{stmts=[
        let_stmt ~scope_name: "a" "a" [value_stmt "10" (BaseTypes.int)];
        let_stmt ~scope_name: "a$2" "a" [value_stmt "str" (BaseTypes.str)]
    ]} in
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