open Base
open Common

type t = {
    source: string;
    modules: string list;
    name: string;
}

let to_string id = "\"%s\"%s.%s" %% [
    id.source; 
    (match id.modules with
    | [] -> ""
    | mods -> "." ^ (String.concat ~sep: "." mods));
    id.name]

let empty = {source = ""; modules = []; name = ""}

let equals a b = 
    String.equal a.source b.source
    && (String.equal a.name b.name)
    && (List.equal String.equal a.modules b.modules)

let make source modules name = {source; modules; name}

let local name = make "" [] name