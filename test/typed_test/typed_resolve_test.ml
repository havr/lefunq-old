(* open Common *)
open Base
open Typed

let check_result args_expect args_got = 
    List.zip_exn args_expect args_got
    |> List.iter ~f: (fun (expect, got) ->
        if not @@ Param.equals expect got then
            Alcotest.fail @@ String.concat ~sep: " " [
                "arg mismatch:";
                Param.to_string expect;
                "!=";
                Param.to_string got;
            ]
    ) 

let test ~given ~expect =
    (* TODO: make TypeNamer a function *)
    let gen = Type_util.make_tempvar_gen "t" in
    let typed = Resolve.type_params gen given in
    check_result expect typed

let ident_param ?typ name = Param.{shape = Param.Name Param.{resolved=name; given=name}; type'=Option.value ~default: Type.Unknown typ}

let simple_name () =
    test 
    ~given: [
        ident_param "x"
    ]
    ~expect: [
        ident_param ~typ: (Type.Var "t0") "x"
    ]

let simple_tuple () =
    test 
    ~given: [
        Param.{shape = Param.Tuple [
            ident_param "a";
            ident_param "b";
            ident_param "c";
        ]; type'=Type.Unknown}
    ]
    ~expect: [
        Param.{
            shape=Param.Tuple [
                ident_param ~typ: (Type.Var "t0") "a";
                ident_param ~typ: (Type.Var "t1") "b";
                ident_param ~typ: (Type.Var "t2") "c";
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