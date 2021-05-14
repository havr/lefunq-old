(* TODO: remove, there is no dedicated resolve anymore *)
(* Move shadowing check somewhere else *)

(* open Common
open Helper
open Typed

let pp_compare pp expect got =
    let pp_expect = Pp.to_string [pp expect] in
    let pp_got = Pp.to_string [pp got] in
    Alcotest.(check string) pp_expect pp_got

let shadowing () =
    let input = [
        let_stmt "a" [value_stmt "10" (Base_types.int)];
        let_stmt "a" [value_stmt "str" (Base_types.str)]
    ] in
    let input_block = (Block.{stmts=input; range = Span.empty_range}) in
    let global = Resolver.Scope.root "" in
    let ctx = Resolve.Ctx.make ~resolve_source: (fun _ -> (raise Common.Unreachable)) 
        (Resolver.Scope.toplevel global) in
    let resolved = Resolve.block ctx input_block in
    let expect = Block.{
        range = Span.empty_range;
        stmts = [
            let_stmt ~scope_name: "a" "a" [value_stmt "10" (Base_types.int)];
            let_stmt ~scope_name: "a$2" "a" [value_stmt "str" (Base_types.str)]
        ]
    } in
    pp_compare (Typed.Node.Print_node.block) expect resolved

let tests = [
    "shadowing", `Quick, shadowing
] *)