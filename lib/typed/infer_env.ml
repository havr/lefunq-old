open Common
open Base

type t = Type.scheme StringMap.t

let make () = Map.empty(module String)

let empty = Map.empty(module String)

let add env name scheme = 
    Map.set env ~key:name ~data:scheme

let singleton name scheme = add empty name scheme

let join s n = Map.fold n ~init:(s) ~f:(fun ~key ~data env -> add env key data)

let diff new_env prev_env = Map.filteri new_env ~f:(fun ~key ~data:_ -> not @@ Map.mem prev_env key)

let free_vars env = 
        Map.to_alist env
    |> List.map ~f: (fun (_, sch) -> 
        Set.diff (Type.free_vars Type.(sch.typ)) (Util.StringSet.from_list sch.constr)
    ) 
    |> Set.union_list(module String)
    
let to_string env = Map.to_alist env
    |> List.map ~f: (fun (k, v) -> k ^ "=" ^ (Type.scheme_to_string v))
    |> String.concat ~sep: ";"

let apply_substs substs env = 
    Map.map env ~f: (fun v -> Subst.apply_to_scheme substs v)

