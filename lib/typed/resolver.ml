open Base
open Common

module Ident = struct
    type t = {
        scheme: Type.scheme;
        scope_name: string
    }
end


module Global = struct
    type binding = {
        scheme: Type.scheme;
        global_name: string;
    }

    type t = {
        parent: t option;
        modname: string;
        mutable bindings: binding StringMap.t;
        mutable sh_bindings: int StringMap.t;
    }

    let root () = {
        parent = None;
        modname = "";
        bindings = Map.empty(module String);
        sh_bindings = Map.empty(module String);
    }

(*todo: toplevel tests *)
(*todo: connect typed to the pipeline (errors) *)
(*todo: let recs to compile existing code *)
    let add_binding res name scheme = 
        let scope_idx = Map.find res.sh_bindings name 
            |> Option.value ~default:1
        in
        let global_prefix = match res.modname with
        | "" -> ""
        | modname -> modname ^ "."  ^ name
        in
        let global_suffix = match scope_idx with
        | 1 -> ""
        | n -> "$" ^ (Int.to_string n)
        in
        let global_name = global_prefix ^ name ^ global_suffix in
        (* TODO: generalize as a function *)
        res.bindings <- Map.set res.bindings ~key: name ~data: {
            scheme;
            global_name
        };
        (* TODO: make ordered map orsomething *)
        res.sh_bindings <- Map.set res.sh_bindings ~key: name ~data: (scope_idx + 1);
        global_name


    let rec lookup resolver name = 
        match Map.find resolver.bindings name with
        | Some m -> Some (`Global m)
        | None -> match resolver.parent with
            | Some p -> lookup p name
            | None -> None
end

module Local = struct 
    type resolved = {
        scope_name: string;
        scheme: Type.scheme option;
    }

    type t = {
        parent: [`Local of t | `Global of Global.t | `None];
        scope: string;
        mutable names: resolved StringMap.t;
        mutable shadowing: int StringMap.t;
    }

    let make global = {
        parent = `Global global;
        scope = "";
        names = Map.empty(module String);
        shadowing = Map.empty(module String)
    }

    let add_name res name scheme = 
        let scope_idx = Map.find res.shadowing name 
            |> Option.value ~default:1
        in
        let scope_suffix = match scope_idx with
        | 1 -> ""
        | n -> "$" ^ (Int.to_string n)
        in

        let scope_preffix = match res.scope with
        | "" -> ""
        | scope -> scope ^ "."
        in

        let scope_name = scope_preffix ^ name ^ scope_suffix in
        res.shadowing <- Map.set res.shadowing ~key: name ~data: (scope_idx + 1);
        res.names <- Map.set res.names ~key: name ~data: {
            scope_name;
            scheme
        };
        scope_name

    let sub ~name parent = {
        parent = `Local parent;
        scope = parent.scope ^ "." ^ name;
        names = Map.empty(module String);
        shadowing = Map.empty(module String)
    }

    let rec lookup resolver name = 
        match Map.find resolver.names name with
        | Some result -> Some (`Local result)
        | None ->
            match resolver.parent with
            | `Local parent -> lookup parent name
            | `Global global -> Global.lookup global name
            | `None -> None
end
