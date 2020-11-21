(* 
open Core_kernel
open Node

let convert_int value =  Sexp.Atom value.int
let convert_str value = Sexp.Atom ("`" ^ value.str ^ "`")
let convert_ident value = Sexp.Atom (value.ident)

let convert_expr _ = Sexp.Atom "xyu"
    match expr with
    | IntValue int -> convert_int int
    | StrValue str -> convert_str str
    | IdentVaue ident -> convert_ident ident
    | ApplyExpr apply -> convert_apply apply
 and convert_apply apply = 
    let open Node in
    let fn' = convert_expr apply.apply_fn in
    let values' = List.map ~f:convert_expr apply.apply_args in
    Sexp.List (fn' :: values') *)
