open Common
open Base

(* let foo = {|"${1}".${2}|} %% [id.source; id.name]

let (%%) str args = 
    let longest = List.fold args ~init:0 ~f:(fun len str -> max len @@ String.length str) in
    let result = Buffer.create (String.length str + longest * (List.length args)) in
    let next_var str =
        if String.is_empty str then
            ("", None)
        else if String.prefix ~prefix: "\$" str then *)

        
module Id = struct 
    type t = {
        source: string;
        name: string;
    }
    let to_string id = "\"" ^ id.source ^ "\"." ^ id.name
    let equals a b = String.equal a.source b.source
        && (String.equal a.name b.name)

    let make source name = {source; name}
end

module Binding = struct
    type t = {
        exposed: Id.t;
        internal: Id.t;
        scheme: Type.scheme
    }

    let make ~internal exposed scheme = {exposed; internal; scheme}
end

module TypeDef = struct 
    type t = unit
end

module Module = struct
    type t = {
        id: Id.t;
        bindings: Binding.t StringMap.t;
        modules: t StringMap.t;
        types: TypeDef.t StringMap.t
    }

    let empty id = {
        id;
        bindings = Map.empty(module String);
        modules = Map.empty(module String);
        types = Map.empty(module String);
    }
end
