open Common
open Base

module Id = struct 
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

    let equals a b = String.equal a.source b.source
        && (String.equal a.name b.name)
        && (List.equal String.equal a.modules b.modules)

    let make source modules name = {source; modules; name}
end

module Resolved = struct 
    type t = {
        resolved: Id.t option;
        given: string
    }

    let make given resolved = {given; resolved}

    let equal a b = (match a.resolved, b.resolved with 
        | Some af, Some bf -> Id.equals af bf
        | _ -> false
    ) && (String.equal a.given b.given)

    let to_string r = 
        let abs = match r.resolved with 
            | Some abs -> Id.to_string abs
            | None -> "<unresolved>"
        in r.given ^ "(" ^ abs ^ ")"

    let equal_path a b = (match List.length a = List.length b with
        | true -> List.zip_exn a b |> List.map ~f:(fun (a, b) -> equal a b) 
            |> List.find ~f:not
            |> Option.is_none
        | false -> false
    )
end

module Qualified = struct 
    type t = {
        name: Resolved.t;
        path: Resolved.t list
    }

    let make name path = {name; path}

    let just_name given resolved = make Resolved.{given; resolved} []

    let given_names q = (List.map q.path ~f:(fun n -> n.given)) @ [q.name.given] 

    let given q = given_names q 
        |> String.concat ~sep: "."

    let from_string name = 
        let name, path = String.split ~on:'.' name 
            |> Util.Lists.last_rest 
        in {
            name = Resolved.make name None;
            path = List.map path ~f: (fun name -> Resolved.make name None)
        }

    let to_string q = 
        (q.path @ [q.name]) 
        |> List.map  ~f: Resolved.to_string
        |> String.concat ~sep: "/"
    
    let append res qual = {qual with path = res :: qual.path}

    let equal a b = (Resolved.equal a.name b.name) && (List.equal Resolved.equal a.path b.path)

    let resolve_name id qual = {qual with name = Resolved.{qual.name with resolved = Some(id)}}
end
(*

let name = [`name, "foo"; (`name 10), 10] |> foo#bar
Foo.bar &: name
*)