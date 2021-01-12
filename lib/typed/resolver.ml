open Base
open Common

module Ident = struct
    type t = {
        scheme: Type.scheme;
        scope_name: string
    }
end


module Global = struct
    type t = {
        parent: t option;
        bindings: Type.scheme StringMap.t;
        sh_bindings: int StringMap.t;
    }

    let root () = {
        parent = None;
        bindings = Map.empty(module String);
        sh_bindings = Map.empty(module String);
    }

(*todo: toplevel tests *)
(*todo: connect typed to the pipeline (errors) *)
(*todo: let recs to compile existing code *)
    let add_binding resolver name scheme = 
        let scope_idx = Map.find resolver.sh_bindings name 
            |> Option.value ~default:1
        in
        let scope_suffix = match scope_idx with
        | 1 -> ""
        | n -> "$" ^ (Int.to_string n)
        in
        let scope_name = name ^ scope_suffix in
        (* TODO: generalize as a function *)
        let bindings' = Map.set resolver.bindings ~key: scope_name ~data: scheme in
        let shadowing' = Map.set resolver.sh_bindings ~key: name ~data: (scope_idx + 1) in
        let resolver' = {resolver with bindings = bindings'; sh_bindings = shadowing'} in
        (scope_name, resolver')


    let rec lookup resolver name = 
        match Map.find resolver.bindings name with
        | Some m -> Some (`Global m)
        | None -> match resolver.parent with
            | Some p -> lookup p name
            | None -> None
end

module Local = struct 
    type t = {
        parent: [`Local of t | `Global of Global.t | `None];
        scope: string;
        names: string StringMap.t;
        idents: Ident.t StringMap.t;
        shadowing: int StringMap.t;
    }

    let make global = {
        parent = `Global global;
        scope = "";
        names = Map.empty(module String);
        idents = Map.empty(module String);
        shadowing = Map.empty(module String)
    }

    let add_name local_resolver name scheme = 
        let scope_idx = Map.find local_resolver.shadowing name 
            |> Option.value ~default:1
        in
        let scope_suffix = match scope_idx with
        | 1 -> ""
        | n -> "$" ^ (Int.to_string n)
        in

        let scope_preffix = match local_resolver.scope with
        | "" -> ""
        | scope -> scope ^ "."
        in

        let scope_name = scope_preffix ^ name ^ scope_suffix in
        let shadowing' = Map.set local_resolver.shadowing ~key: name ~data: (scope_idx + 1) in
        let idents' = Map.set local_resolver.idents ~key: name ~data: Ident.{
            scheme;
            scope_name 
        } in let resolver' = {local_resolver with idents = idents'; shadowing = shadowing'} in
        (scope_name, resolver')

    let sub ~name parent = {
        parent = `Local parent;
        scope = parent.scope ^ "." ^ name;
        names = Map.empty(module String);
        idents = Map.empty(module String);
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
