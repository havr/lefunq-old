open Common
open Base

type named_param = 
    {is_optional: bool; name: string; named_typ: t}

and param = 
    | PosParam of t
    | NamedParam of named_param
    | NamedBlock of named_param list
and t = Var of string 
    | Simple of (string * t list)
    | Lambda of (param * t)
    | Tuple of (t list)
    | Unit
    | Unknown
    | Invalid

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
    | typ :: rest -> Lambda (PosParam (typ), make_typ rest)

in make_scheme constr (make_typ args)

let rec to_string = function
| Unit -> "()"
| Invalid -> "<invalid>"
| Unknown -> "<unknown>"
| Var v -> v
| Simple (v, params) -> 
    let str_params = (match params with 
    | [] -> "" 
    | params -> params 
        |> List.map ~f: to_string 
        |> String.concat ~sep: " "
        |> (^) " "
    ) in v ^ str_params
| Tuple vs ->
    let concat = List.map vs ~f:to_string 
    |> String.concat ~sep: ", " in
    "(" ^ concat ^ ")"
| Lambda (v, n) ->
    let pos = function
        | Lambda lam -> "(" ^ (to_string (Lambda lam)) ^ ")"
        | p -> to_string p
    in
    let named_param {is_optional; name; named_typ} = 
        (if is_optional then name ^ "?" else name) ^ ":" ^ (pos named_typ)
    in
    let param = function
        | PosParam p -> pos p
        | NamedParam b -> 
            named_param b 
        | NamedBlock b -> (
            List.map ~f: (named_param) b 
                |> String.concat ~sep:" -> "
        )
    in
    let ns = to_string n  in
    (param v) ^ " -> " ^ ns

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
| (Lambda (PosParam ah, at), Lambda (PosParam bh, bt)) ->
    equals ah bh && equals at bt
| (Lambda (NamedBlock ab, at), Lambda (NamedBlock bb, bt)) ->
    (List.equal phys_equal ab bb) && equals at bt
| Unit, Unit -> true
| Invalid, Invalid -> true
| _, _ -> false

let rec free_vars = function
| Invalid -> Set.empty (module String)
| Unit -> Set.empty (module String)
| Unknown -> Set.empty (module String)
| Simple _  -> Set.empty (module String)
(* TODO: more convenient *)
| Var v -> Set.singleton (module String)  v
| Tuple t -> List.fold t ~init: (Set.empty (module String)) ~f: 
    (fun result typ -> Set.union result (free_vars typ))
| Lambda ((PosParam a), b) -> Set.union (free_vars a) (free_vars b)
| Lambda ((NamedParam p), b) -> 
    free_vars p.named_typ
    |> Set.union (free_vars b)
| Lambda ((NamedBlock a), b) -> 
    List.map a ~f: (fun p -> free_vars p.named_typ)
    |> List.fold ~init: (Set.empty(module String)) ~f: (Set.union)
    |> Set.union (free_vars b)

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
    (* let rec unroll (arg, ret) =
        match ret with
        | Lambda ret -> arg :: (unroll ret)
        | t -> [arg; t] *)

    let make_positional args = 
        let rec loop = function 
            | [] -> raise (Invalid_argument "making a lambda with no arguments")
            | h :: [] -> h 
            | h :: t -> Lambda (PosParam h, loop t)
        in loop args

    let make args result = 
        let rec loop = function 
            | [] -> raise (Invalid_argument "making a lambda with no arguments")
            | h :: [] -> Lambda(h, result)
            | h :: t -> Lambda (h, loop t)
        in loop args

    let split_head (param, next) =
        match param with
        | PosParam p -> (`PosParam p, next)
        | NamedParam p -> (`NamedParam p, next)
        | NamedBlock (h :: rest) -> 
            (`NamedParam h, match rest with 
                | [] -> next 
                | t -> Lambda (NamedBlock(t), next))
        | NamedBlock [] -> (raise Common.Unreachable)
end
