open Common
open Base

type t = {
    tempvar: Type.tempvar_gen;
    weakvar: Type.tempvar_gen;
    scope: Scope.t;
    resolve_source: Infer_using.resolve_source_fn;
    errors: Errors.t list ref;
    env: Infer_env.t ref;
    substs: Type.t StringMap.t ref
}


let make ~resolve_source ~scope ~env = {
    env;
    scope;
    resolve_source;
    errors = ref [];
    tempvar = Type.make_tempvar_gen "_t";
    weakvar = Type.make_tempvar_gen "_weak";
    substs = ref (Map.empty (module String))
}

let add_env ~ctx name value =
    ctx.env := Infer_env.add !(ctx.env) name value

let add_subst ~ctx name value =
    ctx.substs := Subst.add !(ctx.substs) name value

let lookup_env ~ctx name = Map.find_exn !(ctx.env) name

let sub_local ctx = {ctx with scope = Scope.sub_local ctx.scope}

let add_error ~ctx error = 
    ctx.errors := !(ctx.errors) @ [error]

let unify_var ~ctx v typ =
    ctx.substs := Subst.combine (Subst.single v typ) !(ctx.substs)

let try_unify ~ctx ~error base new' =
    let substs = !(ctx.substs) in
    let (substs', errors) = Infer_algo.unify (Subst.apply substs base) (Subst.apply substs new') in
    ctx.substs := Subst.combine substs' substs;
    match errors with
    | [] -> true
    | _ -> 
        ctx.errors := !(ctx.errors) @ [error];
        false

let unify ~ctx ~error base new' = ignore @@ try_unify ~ctx ~error base new'

let apply ~ctx typ = Subst.apply !(ctx.substs) typ