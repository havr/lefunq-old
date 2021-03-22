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

    let rec lookup_modu modu = function
        | [] -> Some Symbol.Module.{modu = Some modu; typedef = None; binding = None}
        | name :: [] -> Map.find Symbol.Module.(modu.exposed) name
        | name :: rest ->
            (match Map.find modu.exposed name with
            | None -> None
            | Some exposed -> (match Symbol.Module.(exposed.modu) with 
                | None -> None
                | Some m -> lookup_modu m rest
            ))

    (* let lookup_resol modu = function
        | [] -> Some modu
        | name :: rest ->
            (match Map.find Symbol.Module.(modu.exposed) name with
            | None -> None
            | Some {modu = None; _} -> None
            | Some {modu = Some m; _} -> lookup_modu m (name :: rest)) *)

    type t = {
        parent: t option;
        source: string;
        modules: string list;
        namer: string -> string;
        mutable names: Symbol.Module.exposed StringMap.t;
    }

    let root source = {
        parent = None;
        source = source;
        modules = [];
        names = Map.empty(module String);
        namer = namer ""
    }

    let set_resolution scope name fn = 
        let existing = match Map.find scope.names name with
        | None -> Symbol.Module.{modu = None; typedef = None; binding = None}
        | Some value -> value
        in scope.names <- Map.set scope.names ~key: name ~data: (fn existing)
        
        (* in let new' = {
            modu = replace ext.modu existing.modu;
            typedef = replace ext.typedef existing.typedef;
            binding = replace ext.binding existing.binding;
        } in scope.names <- Map.set scope.names ~key: name ~data: new' *)

    let make_id scope name =
        let scope_name = scope.namer name in
        Symbol.Id.{
            source = scope.source;
            modules = scope.modules;
            name = scope_name;
        }

    let toplevel scope = {
        parent = Some scope;
        source = "";
        modules = scope.modules;
        names = Map.empty(module String);
        namer = namer ""
    }

    let sub_local scope = {
        parent = Some scope;
        source = "";
        modules = scope.modules;
        names = Map.empty(module String);
        namer = scope.namer
    }

    let sub_module name scope = {
        parent = Some scope;
        source = scope.source;
        modules = scope.modules @ [name];
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

    let add_module scope given_name exposed =
        let id = make_id scope given_name in
        let modu = Symbol.Module.{
            id; exposed
        } in
        set_resolution scope given_name (fun res -> { res with
            modu = Some modu;
        });
        id

    let add_binding scope exposed scheme =
        let id = make_id scope exposed in
        (* TODO: what when I re-expose stuff? *)
        set_resolution scope exposed (fun res -> { res with
            binding = Some Symbol.Binding.{
                id;
                scheme
            }
        });
        id

    let implant_binding scope exposed scheme =
        let id = Symbol.Id.make "" [] exposed in
        set_resolution scope exposed (fun res -> { res with
            binding = Some Symbol.Binding.{
                id;
                scheme
            }
        });
        id

    let import scope name ext = 
        let replace n old = if Option.is_some n then n else old in
        set_resolution scope name Symbol.Module.(fun existing -> {
            modu = replace ext.modu existing.modu;
            typedef = replace ext.typedef existing.typedef;
            binding = replace ext.binding existing.binding;
        })
        

    let lookup_exposed names exposed = 
        let rec lookup_exposed' result names = function
            | [] -> raise (Invalid_argument "no path is provided")
            | name :: [] -> 
                Symbol.Resolved.(name.given) 
                |> Map.find names 
                |> Option.map ~f:(fun m -> m, List.rev result)
            | name :: rest -> (
                match Map.find names name.given with
                | None -> None
                | Some exposed -> (match Symbol.Module.(exposed.modu) with
                    | None -> None
                    | Some modu -> 
                        let resolved = (Symbol.Resolved.make name.given (Some modu.id)) in
                        lookup_exposed' (resolved :: result) Symbol.Module.(modu.exposed) rest
                )
            )
        in lookup_exposed' [] names exposed

    let mprint m = Map.iteri m ~f: (fun ~key ~data -> match Symbol.Module.(data.binding) with 
        | None -> ()
        | Some m -> Common.log [key; "="; m.id.name]
    )

    let rec lookup scope path = 
        match path with
        | [] -> raise (Invalid_argument "expected qualified name, got empty one")
        | name :: rest ->
            match Map.find scope.names Symbol.Resolved.(name.given) with
            | Some exposed ->
                (match rest with
                    | [] -> Some (exposed, [])
                    | rest -> (match exposed.modu with 
                        | Some m -> lookup_exposed m.exposed rest 
                            |> Option.map ~f: (fun (value, path) -> (value, (Symbol.Resolved.make name.given (Some m.id)) :: path))
                        | None -> None
                    )
                )
            | None -> 
                match scope.parent with 
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
