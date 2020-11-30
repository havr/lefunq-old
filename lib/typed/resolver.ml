open Base

module IdentMap = Map.M(String)

type t = {
    parent: t option;
    scope: string;
    names: string IdentMap.t
}

let local () = {
    parent = None;
    scope = "";
    names = Map.empty(module String)
}

let add_name resolver name = { resolver with 
    names = Map.set ~key: name ~data: name resolver.names 
}

let sub ~name parent = {
    parent = Some parent;
    scope = parent.scope ^ "." ^ name;
    names = Map.empty(module String)
}

let rec lookup resolver name = 
    match Map.find resolver.names name with
    | Some result -> Some result
    | None ->
        match resolver.parent with
        | Some parent -> lookup parent name
        | None -> None
