open Common
open Base

type t = Var of string 
    | Simple of (string * t list)
    | Lambda of (t * t)
    | Tuple of (t list)
    | Unit
    | Unknown

type constr = (string)

type scheme = {
    constr: constr list;
    typ: t;
}

let make_scheme constr typ = {constr; typ}

let unknown_scheme = make_scheme [] Unknown

let lambda ?(constr=[]) args = 
    let rec make_typ = function
    | [] -> raise (Unreachable)
    | typ :: [] -> typ
    | typ :: rest -> Lambda (typ, make_typ rest)

in make_scheme constr (make_typ args)

let rec to_string = function
| Unit -> "()"
| Unknown -> "???"
| Var v -> "#" ^ v
| Simple (v, params) -> "%s %s" %% [v; params |> List.map ~f: to_string |> String.concat ~sep: " "]
| Tuple vs ->
    let concat = List.map vs ~f:to_string 
    |> String.concat ~sep: ", " in
    "(" ^ concat ^ ")"
| Lambda (v, n) ->
    let vs = match v with
        | Lambda _ -> "(" ^ (to_string v) ^ ")" 
        | _ -> to_string v
    in
    let ns = to_string n  in
    vs ^ " -> " ^ ns

let scheme_to_string scheme =
    let constrs = match scheme.constr with
    | [] -> ""
    | constrs -> "forall " ^ (String.concat ~sep: ", " constrs) ^ ". "
    in
    constrs ^ (to_string scheme.typ)

let rec equals a b = match (a, b) with
| (Var av, Var bv) -> String.equal av bv
| (Simple (av, ap), Simple (bv, bp)) -> 
    String.equal av bv && (List.length ap = List.length bp) && (
        List.zip_exn ap bp 
        |> List.find ~f: (fun (a, b) -> not @@ equals a b) 
        |> Option.is_some 
        |> not
    )
| (Tuple av, Tuple bv) -> 
    if List.length av = List.length bv then 
        let count_equals = List.zip_exn av bv 
            |> List.filter ~f: (fun (av, bv) -> equals av bv) 
            |> List.length in
        count_equals = List.length av
    else false
| (Lambda (ah, at), Lambda (bh, bt)) ->
    equals ah bh && equals at bt
| _ -> false

let rec free_vars = function
| Unit -> Set.empty (module String)
| Unknown -> Set.empty (module String)
| Simple _  -> Set.empty (module String)
(* TODO: more convenient *)
| Var v -> Set.singleton (module String)  v
| Tuple t -> List.fold t ~init: (Set.empty (module String)) ~f: 
    (fun result typ -> Set.union result (free_vars typ))
| Lambda (a, b) -> Set.union (free_vars a) (free_vars b)

let lists_equal compare a b =
    List.length a = List.length b
    && (
        List.zip_exn (List.sort ~compare a) (List.sort ~compare b)
        |> List.find ~f:(fun (a, b) -> compare a b <> 0)
        |> Option.is_none
    )

let scheme_equals a b =
    equals a.typ b.typ && lists_equal (String.compare) a.constr b.constr

module Lambda = struct 
    let rec unroll (arg, ret) =
        match ret with
        | Lambda ret -> arg :: (unroll ret)
        | t -> [arg; t]

    let rec make = function 
    | [] -> raise (Invalid_argument "making a lambda with no arguments")
    | h :: [] -> h
    | h :: t -> Lambda (h, make t)
end
