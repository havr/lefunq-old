open Common
open Base



module Resolved_name = struct 
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
        name: Resolved_name.t;
        path: Resolved_name.t list
    }

    let make name path = {name; path}

    let just_name given resolved = make Resolved_name.{given; resolved} []

    let given_names q = (List.map q.path ~f:(fun n -> n.given)) @ [q.name.given] 

    let given q = given_names q 
        |> String.concat ~sep: "."

    let from_string name = 
        let name, path = String.split ~on:'.' name 
            |> Util.Lists.last_rest 
        in {
            name = Resolved_name.make name None;
            path = List.map path ~f: (fun name -> Resolved_name.make name None)
        }

    let to_string q = 
        (q.path @ [q.name]) 
        |> List.map  ~f: Resolved_name.to_string
        |> String.concat ~sep: "/"
    
    let append res qual = {qual with path = res :: qual.path}

    let equal a b = (Resolved_name.equal a.name b.name) && (List.equal Resolved_name.equal a.path b.path)

    let resolve_name id qual = {qual with name = Resolved_name.{qual.name with resolved = Some(id)}}
end