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
        modules: string list;
        name: string;
    }
    let to_string id = "\"%s\"%s.%s" %% [
        id.source; 
        (match id.modules with
        | [] -> ""
        | mods -> "." ^ (String.concat ~sep: "." mods));
        id.name]

    let equals a b = String.equal a.source b.source
        && (String.equal a.name b.name)
        && (List.equal String.equal a.modules b.modules)

    let make source modules name = {source; modules; name}
end

module Resolved = struct 
    type t = {
        absolute: Id.t option;
        given: string
    }

    let make given absolute = {given; absolute}

    let equal a b = (match a.absolute, b.absolute with 
        | Some af, Some bf -> Id.equals af bf
        | _ -> false
    ) && (String.equal a.given b.given)

    let to_string r = 
        let abs = match r.absolute with 
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
        

module Binding = struct
    type t = {
        id: Id.t;
        scheme: Type.scheme
    }

    let make id scheme = {id; scheme}
end

module TypeDef = struct 
    type t = unit
end

module Module = struct
    type exposed = {
        binding: Binding.t option;
        modu: t option;
        typedef: TypeDef.t option
    }

    and t = {
        id: Id.t;
        exposed: exposed StringMap.t;
    }

    let empty_exposed = {binding = None; modu = None; typedef = None}

    let make id exposed = {id; exposed }

    let empty id = make id (Map.empty(module String))

    let rec lookup exposed = function
    | [] -> raise (Invalid_argument "no path provided")
    | name :: [] ->
        Map.find exposed name
    | modu :: rest -> (
        match Map.find exposed modu with
        | None -> None
        | Some exposed -> (
            match exposed.modu with 
            | None -> None
            | Some m -> lookup m.exposed rest)
    )
end
