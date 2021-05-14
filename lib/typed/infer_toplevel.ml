open Base
open Common

let binding ~resolve_source scope n =
    let env = Infer_env.empty in
    let ctx = Infer_ctx.make ~resolve_source ~scope: (Scope.sub_local scope) ~env: (ref env) in
    let params = Infer_expr.infer_params ~ctx Node.Let.(n.params) in
    let rec_scope_name = if Node.Let.(n.is_rec) then (
        let rec_scheme = Infer_expr.generalize_lambda_scheme !(ctx.env) params (ctx.tempvar()) in
        let scope_name = Scope.add_global_binding scope n.given_name rec_scheme in
        Some scope_name
    ) else (
        None
    ) in

    let scheme, params, block = Infer_expr.let_lambda ~ctx env params n.block in
    let scope_name = (match rec_scope_name with
    | Some sn ->
        Scope.set_global_binding scope Node.Let.(n.given_name) scheme;
        sn
    | None ->
        Scope.add_global_binding scope Node.Let.(n.given_name) scheme
    ) in

    let errors = List.map !(ctx.errors) ~f: (Subst.apply_to_error !(ctx.substs)) in
    {n with scope_name = scope_name.name; scheme = Some scheme; params; block}, errors

let expose modu_id entries = 
    (* Common.log["exposing:"; Typed_common.Id.(modu_id.name); "<<";Common.stacktrace ()]; *)
    let modules = if String.is_empty Id.(modu_id.name) then [] else (modu_id.modules @ [modu_id.name]) in
    let make_id scope_name = Id.make Id.(modu_id.source) modules scope_name in
    let open Node in
    entries
    |> List.filter ~f: (function 
        | Module.Typedef m -> not @@ String.is_prefix ~prefix:"_" m.name.value
        | Module.Module m -> not @@ String.is_prefix ~prefix:"_" m.given_name
        | Module.Binding b -> not @@ String.is_prefix ~prefix:"_" b.given_name
        | Module.Using _ -> false
    )
    |> List.fold ~init: (Resolved.Module.empty modu_id) ~f: (fun sym node -> 
        let given_name = (match node with 
                | Module.Module m -> m.given_name
                | Module.Binding b -> b.given_name
                | Module.Typedef t -> t.name.value
                | _ -> raise Common.Unreachable) in 
        { sym with exposed = Map.update sym.exposed given_name
            ~f: (fun exposed -> 
                let exposed = (Option.value exposed ~default: (Resolved.Module.empty_exposed)) in
                (match node with 
                | Module.Module m -> 
                    { exposed with modu = Some (Resolved.Module.make (make_id m.scope_name) m.exposed) } 
                | Module.Binding b -> 
                    { exposed with binding = Some (Resolved.Binding.make (make_id b.scope_name) (Option.value ~default: Type.unknown_scheme b.scheme)) } 
                | Module.Typedef t ->
                    let params = List.map t.params ~f: (fun param -> Resolved.Typedef.{var = param.var.value}) in
                    let typedef = Resolved.Typedef.make (make_id t.scope_name) params t.def in
                    { exposed with typedef = Some typedef } 
                | _ -> raise Common.Unreachable))
        } 
    ) 

let typedef resolver td =
    let open Node in
    let params = List.map Typedef.(td.params) ~f: (fun {var} -> Resolved.Typedef.{var = var.value}) in
    let scope_name = Scope.add_typedef resolver td.name.value params td.def in
    Ok scope_name

let rec modu ~resolve_source resolver id m = 
    (* TODO: Resolved.Module / Typedef.Module / Def.Module *)
    (* TODO: Resolved.Id / some global id *)
    (* s/typedefs/typs in Resolved.t: typedef is type definition *)
    let open Node in
    match try_map Node.Module.(m.entries) ~f: (function
        | Node.Module.Typedef td -> 
            (match typedef resolver td with 
                | Ok id -> Ok (Module.Typedef {td with scope_name = id.name})
                | Error e -> Error e
            )
        | Node.Module.Module m -> 
            let id = Scope.make_id resolver m.given_name in
            let m = {m with scope_name = id.name} in
            let res = Scope.sub_module id.name resolver in
            (match modu ~resolve_source res id m with
            | (Error e) -> Error e
            | (Ok tm) -> 
                let modu = Resolved.Module.{
                    id; exposed = Module.(tm.exposed)
                } in
                Scope.set_resolution resolver m.given_name (fun res -> { res with
                    modu = Some modu;
                });
                Ok (Module.Module tm))
        | Node.Module.Binding b ->
            let (typ, errs) = binding ~resolve_source resolver b in
            (match errs with 
                | [] -> Ok (Module.Binding typ)
                | _ -> Error errs)
        | Node.Module.Using im -> 
            let (import, errs) = Infer_using.using resolve_source resolver im in
            (match errs with
            | [] -> Ok (Module.Using import)
            | errs -> Error errs)
    ) with
        | Error errors -> Error errors
        (* TODO: return map not module *)
        | Ok entries -> let exposed = expose id entries in
            Ok (Module.{ m with entries; exposed = exposed.exposed })

let root ~builtin ~source ~resolve_source m = 
    let resolver = Scope.root source in
    Map.iteri builtin ~f:(fun ~key ~data ->
        Scope.import resolver key data
    );
        (* TODO: pass name *)
    let tf = Transform.root m in 
    modu ~resolve_source resolver (Id.make source [] "") tf