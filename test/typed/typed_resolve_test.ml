(* open Common *)
open Base
open Typed

let check_result args_expect args_got = 
    List.zip_exn args_expect args_got
    |> List.iter ~f: (fun (expect, got) ->
        if not @@ Arg.equals expect got then
            Alcotest.fail @@ String.concat ~sep: " " [
                "arg mismatch:";
                Arg.to_string expect;
                "!=";
                Arg.to_string got;
            ]
    ) 

let test ~given ~expect =
    (* TODO: make TypeNamer a function *)
    let gen = make_tempvar_gen "t" in
    let typed = Resolve.type_args gen given in
    check_result expect typed

let simple_name () =
    test 
    ~given: [
        Arg.{shape = Arg.Name "x"; type'=Type.Unknown}
    ]
    ~expect: [
        Arg.{shape=Arg.Name "x"; type'=Type.Var "t0"}
    ]

let simple_tuple () =
    test 
    ~given: [
        Arg.{shape = Arg.Tuple [
            Arg.{shape = Arg.Name "a"; type' = Type.Unknown};
            Arg.{shape = Arg.Name "b"; type' = Type.Unknown};
            Arg.{shape = Arg.Name "c"; type' = Type.Unknown};
        ]; type'=Type.Unknown}
    ]
    ~expect: [
        Arg.{
            shape=Arg.Tuple [
                Arg.{shape = Arg.Name "a"; type' = Type.Var "t0"};
                Arg.{shape = Arg.Name "b"; type' = Type.Var "t1"};
                Arg.{shape = Arg.Name "c"; type' = Type.Var "t2"};
            ]; 
            type'=Type.Tuple [Type.Var "t0"; Type.Var "t1"; Type.Var "t2"]
        }
    ]

(* TODO: write tests for it *)
(* TODO: refactor *)

(* TODO: integrate it with lambda-inference *)
(* TODO: make let-inference great again 
    annotate top-level and apply result to the tree
    return and convert errors
    local resolution w/o modules and sigs
    le grand refactor
    (at this point it can type simple nonrec, nonpoly programs)
    let rec
    polytypes: list literals
    ===
    type constructors with primitive types
    with generics
    polymorphic variants
    local modules 
    global modules
    js library / interop with modules
*)

let tests = [
    "simple_name", `Quick, simple_name;
    "simple_tuple", `Quick, simple_tuple
]