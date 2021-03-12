open Base
open Common
module Scope = struct 
    let namer prefix = 
        let sh = ref (Map.empty(module String)) in
        fun name ->
            let curr_idx = match Map.find !sh name with
            | Some idx -> idx + 1
            | None -> 0
            in
            let name' = match curr_idx with
            | 0 -> name
            | n -> prefix ^ name ^ "__" ^ (Int.to_string n)
            in
            sh := Map.set !(sh) ~key: name ~data: curr_idx;
            name'

    type resolution = {
        modu: Symbol.Module.t option;
        typedef: Symbol.TypeDef.t option;
        binding: Symbol.Binding.t option;
    }

    let rec lookup_modu modu = function
        | [] -> Some {modu = Some modu; typedef = None; binding = None}
        | name :: [] ->
            let m = Map.find Symbol.Module.(modu.modules) name in
            let typedef = Map.find modu.types name in
            let binding = Map.find modu.bindings name in
            begin match m, typedef, binding with
            | None, None, None -> None
            | _, _, _ -> Some { modu = m; typedef; binding }
            end
        | name :: rest ->
            match Map.find modu.modules name with
            | None -> None
            | Some m -> lookup_modu m rest

    let lookup_resol resol = function
        | [] -> Some resol
        | name :: rest ->
            match resol.modu with
            | None -> None
            | Some m -> lookup_modu m (name :: rest)

    type fn = string -> resolution option
    type t = {
        parent: t option;
        source: string;
        path: string;
        namer: string -> string;
        mutable names: resolution StringMap.t;
    }

    let root source = {
        parent = None;
        source = source;
        path = "";
        names = Map.empty(module String);
        namer = namer ""
    }

    let set_resolution scope name fn = 
        let existing = match Map.find scope.names name with
        | None -> {modu = None; typedef = None; binding = None}
        | Some value -> value
        in scope.names <- Map.set scope.names ~key: name ~data: (fn existing)
        
        (* in let new' = {
            modu = replace ext.modu existing.modu;
            typedef = replace ext.typedef existing.typedef;
            binding = replace ext.binding existing.binding;
        } in scope.names <- Map.set scope.names ~key: name ~data: new' *)

    let make_id scope name =
        let scope_name = scope.namer name in
        (if String.equal name "fact" then Common.log[Common.stacktrace ()]);
        Common.log["make id"; name; scope_name];
        Symbol.Id.{
            source = scope.source;
            name = if phys_equal scope.path "" then scope_name 
                else scope.path ^ "." ^ scope_name
        }

    let local_root scope = {
        parent = Some scope;
        source = "";
        path = "";
        names = Map.empty(module String);
        namer = namer ""
    }

    let sub_local scope = {
        parent = Some scope;
        source = "";
        path = "";
        names = Map.empty(module String);
        namer = scope.namer
    }

    let sub_module name scope = {
        parent = Some scope;
        source = scope.source;
        path = scope.path ^ "." ^ name;
        names = Map.empty(module String);
        namer = namer ""
    }
 

    let set_binding_scheme scope given_name scheme =
        scope.names <- Map.change scope.names given_name ~f:(function
            | None -> raise (Invalid_argument ("%s not found " %% [given_name]))
            | Some r -> (match r.binding with
                | None -> raise (Invalid_argument ("binding %s not found" %% [given_name]))
                | Some b -> Some {r with binding = Some {b with scheme}}
            )
        )

    let add_binding scope exposed scheme =
        let id = make_id scope exposed in
        (* TODO: what when I re-expose stuff? *)
        set_resolution scope exposed (fun res -> { res with
            binding = Some Symbol.Binding.{
                internal = id;
                exposed = id; (*Symbol.Id.make scope.path exposed;*)
                scheme
            }
        });
        id

    let implant_binding scope exposed scheme =
        let id = Symbol.Id.make "" exposed in
        set_resolution scope exposed (fun res -> { res with
            binding = Some Symbol.Binding.{
                internal = id;
                exposed = Symbol.Id.make scope.path exposed;
                scheme
            }
        });
        id

    let import scope name ext = 
        let replace n old = if Option.is_some n then n else old in
        set_resolution scope name (fun existing -> {
            modu = replace ext.modu existing.modu;
            typedef = replace ext.typedef existing.typedef;
            binding = replace ext.binding existing.binding;
        })

    let rec lookup scope path = 
        match String.split ~on: '.' path with
        | [] -> raise (Invalid_argument "expected qualified name, got empty one")
        | name :: rest ->
            match Map.find scope.names name with
            | Some r -> lookup_resol r rest
            | None -> match scope.parent with 
                | Some ps -> (lookup ps) path
                | None -> None
end
(* 
module Global = struct
    type t = {
        parent: t option;
        scope: Scope.t;
        modname: string;
    }

    let root () = {
        parent = None;
        modname = "";
        scope = Scope.make ();
    }

(*todo: toplevel tests *)
(*todo: connect typed to the pipeline (errors) *)
(*todo: let recs to compile existing code *)
    let import resolver name value = 
        Scope.import resolver.scope name value


    let add_own resolver name value = 
        let scope_idx = Map.find resolver.shadowing name 
            |> Option.value ~default:1
        in
        let global_prefix = match resolver.modname with
        | "" -> ""
        | modname -> modname ^ "."  ^ name
        in
        let global_suffix = match scope_idx with
        | 1 -> ""
        | n -> "$" ^ (Int.to_string n)
        in
        let global_name = global_prefix ^ name ^ global_suffix in
        (* TODO: generalize as a function *)
        resolver.scope <- Map.set resolver.scope ~key: name ~data: value;       (* TODO: make ordered map orsomething *)
        resolver.shadowing <- Map.set resolver.shadowing ~key: name ~data: (scope_idx + 1);
        global_name


    let rec lookup resolver name = 
        match String.split ~on: '.' name with
        | [] -> raise (Invalid_argument "expected qualified name, got empty one")
        | head :: rest ->
            match Map.find resolver.scope head with
            | Some r -> lookup_resol r rest
            | None -> match resolver.parent with
                | Some p -> lookup p name
                | None -> None
end

module Local = struct 
    type t = {
        scope: Scope.t;
        namer: string -> string
    }

    let make _ = {
        scope = Scope.make ();
        namer = Namer.namer "";
    }

    let add_local_binding res name scheme = 
        let name = res.namer name in
        let binding = Scope.add_local_binding 
        Scope.add_local_binding res.scope
        let scope_idx = Map.find res.shadowing name 
            |> Option.value ~default:1
        in
        let scope_suffix = match scope_idx with
        | 1 -> ""
        | n -> "$" ^ (Int.to_string n)
        in

        let scope_preffix = match res.scope_path with
        | "" -> ""
        | scope -> scope ^ "."
        in

        let scope_name = scope_preffix ^ name ^ scope_suffix in
        res.shadowing <- Map.set res.shadowing ~key: name ~data: (scope_idx + 1);
        res.scope <- Map.set res.scope ~key: name ~data: (`Local (Binding {
            scope_name;
            scheme = scheme
        }));
        scope_name

    let sub ~name parent = {
        parent = `Local parent;
        scope_path = parent.scope_path ^ "." ^ name;
        scope = Map.empty(module String);
        shadowing = Map.empty(module String)
    }

    let rec lookup resolver name = 
        match Map.find resolver.scope name with
        | Some result -> Some result
        | None ->
            match resolver.parent with
            | `Local parent -> lookup parent name
            | `Global glob -> begin match Global.lookup glob name with 
                | Some result -> Some (`Global result)
                | None -> None
            end
            | `None -> None

    let rec lookup resolver name = 
        match String.split ~on: '.' name with
        | [] -> raise (Invalid_argument "expected qualified name, got empty one")
        | head :: rest ->
            match Map.find resolver.scope head with
            | Some r -> lookup_resol r rest
            | None -> match resolver.parent with
                | Some p -> lookup p name
                | None -> None
end *)
