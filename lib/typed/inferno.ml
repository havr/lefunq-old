open Common
open Base
open Type_util

type env = Type.scheme StringMap.t

type ctx = {
    tempvar: tempvar_gen;
    mutable errors: Erro.t list;
    mutable env: env;
    mutable substs: Type.t StringMap.t
}

let make_env () = Map.empty(module String)

(* TODO: make_context *)
let make_ctx ~env = {
    env;
    errors = [];
    tempvar = make_tempvar_gen "t";
    substs = Map.empty (module String)
}

open Erro

let free_vars ~ctx typ = Type.free_vars typ
    |> Set.filter ~f: (fun name -> not @@ Map.mem ctx.substs name) 
    |> Set.to_list


let unify ta tb = 
    let errors = ref [] in
    let rec unify' substs ta tb = 
        let unify_lists a b =
            List.fold (List.zip_exn a b) ~init: Subst.empty_subst ~f: (fun result (ia, ib) -> 
                let unified = unify' result ia ib in 
                Subst.combine_substs result unified)
        in
        let mismatch () = 
            let err = TypeMismatch {
                type_expected = ta;
                type_provided = tb;
                range = Span.empty_range
            } in
            errors := !errors @ [err];
            Subst.empty_subst
        in
        match ta, tb with
        | Type.Var va, _ -> 
            Subst.new_subst va tb
        | _, Type.Var vb -> 
            Subst.new_subst vb ta 
        | Type.Simple (a, ap), Type.Simple (b, bp) -> 
            (match String.equal a b with
            | false -> mismatch ()
            | true -> unify_lists ap bp)
        | Type.Tuple tua, Type.Tuple tub ->
            (match List.length tua = List.length tub with
            | false -> mismatch ()
            | true -> unify_lists tua tub)
        | (Type.Lambda (ha, ra), Type.Lambda (hb, rb)) -> (
            let unified = unify' substs ha hb in 
            let combined = Subst.combine_substs substs unified in
            unify' combined (Subst.apply_substs combined ra) (Subst.apply_substs combined rb))
        | Type.Unit, Type.Unit -> Subst.empty_subst
        | _ -> mismatch ();
    in let result = unify' (Subst.empty_subst) ta tb  in
    (result, (!errors))

let type_mismatch_from = List.map ~f: (fun (expect, got) -> 
    TypeMismatch{type_expected = expect; type_provided = got; range = Span.empty_range})

let unify_ctx ~ctx base new' = 
    let (substs', errors) = unify (Subst.apply_substs ctx.substs base) (Subst.apply_substs ctx.substs new') in
    ctx.substs <- Subst.combine_substs (*ctx.substs*) substs' ctx.substs;
    errors
    (* let (substs', errors) = unify (Subst.apply_substs ctx.substs base) (Subst.apply_substs ctx.substs new') in
    ctx.substs <- Subst.combine_substs (*ctx.substs*) substs' ctx.substs; *)

let do_unify ~ctx ~error base new' = 
    match unify_ctx ~ctx base new' with
    | [] -> true
    | _ -> 
        ctx.errors <- ctx.errors @ [error];
        false

let set_env ~ctx name scheme = 
    ctx.env <- Map.set ctx.env ~key:name ~data:scheme

let apply_substs ~ctx typ = Subst.apply_substs ctx.substs typ

let add_subst ~ctx v scheme = 
    ctx.substs <- Subst.combine_substs ctx.substs (Subst.new_subst v scheme) 

let ctx_add_errors ~ctx errors = 
    List.iter errors ~f:(fun error -> ctx.errors <- ctx.errors @ [error])