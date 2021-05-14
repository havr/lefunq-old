open Base
open Common
open Typed_common

let unify ta tb = 
    let errors = ref [] in
    let rec unify' substs ta tb = 
        let unify_lists a b =
            List.fold (List.zip_exn a b) ~init: Subst.empty ~f: (fun result (ia, ib) -> 
                let unified = unify' result ia ib in 
                Subst.combine result unified)
        in
        let mismatch () = 
            let err = Errors.TypeMismatch {
                type_expected = ta;
                type_provided = tb;
                range = Span.empty_range
            } in
            errors := !errors @ [err];
            Subst.empty
        in
        match ta, tb with
        | Type.Var va, _ -> 
            Subst.single va tb
        | _, Type.Var vb -> 
            Subst.single vb ta 
        | Type.Simple (a, ap), Type.Simple (b, bp) -> 
            (match Qualified.equal a.resolved b.resolved with
            | false -> mismatch ()
            | true -> unify_lists ap bp)
        | Type.Tuple tua, Type.Tuple tub ->
            (match List.length tua = List.length tub with
            | false -> mismatch ()
            | true -> unify_lists tua tub)
        | (Type.Lambda la, Type.Lambda lb) -> (
            let lah, lar = Type.Lambda.split_head la in
            let lbh, lbr = Type.Lambda.split_head lb in

            let unified = (match lah, lbh with
            | `PosParam a, `PosParam b ->
                unify' substs a b
            | `NamedParam a, `NamedParam b ->
                if (Bool.equal a.is_optional b.is_optional) && (String.equal a.param_name b.param_name) 
                    then unify' substs a.param_typ b.param_typ 
                    else ( (* TODO: Error *) Subst.empty)
            | `PosParam _, `NamedParam _ ->
                (* TODO: error *)
                Subst.empty;
            | `NamedParam _, `PosParam _ ->
                (* TODO: error *)
                Subst.empty;
            ) in
            let combined = Subst.combine substs unified in
            unify' combined (Subst.apply combined lar) (Subst.apply combined lbr))
        | Type.Unit, Type.Unit -> Subst.empty
        | _ -> mismatch ();
    in let result = unify' (Subst.empty) ta tb  in
    (result, (!errors))