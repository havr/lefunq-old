open Base
open Common
open Typed_common

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
    | [] -> Some Resolved.Module.{modu = Some modu; typedef = None; binding = None}
    | name :: [] -> Map.find Resolved.Module.(modu.exposed) name
    | name :: rest ->
        (match Map.find modu.exposed name with
        | None -> None
        | Some exposed -> (match Resolved.Module.(exposed.modu) with 
            | None -> None
            | Some m -> lookup_modu m rest
        ))

type t = {
    parent: t option;
    source: string;
    modules: string list;
    namer: string -> string;
    mutable names: Resolved.Module.exposed StringMap.t;
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
    | None -> Resolved.Module.{modu = None; typedef = None; binding = None}
    | Some value -> value
    in scope.names <- Map.set scope.names ~key: name ~data: (fn existing)
    
let make_id scope name =
    let scope_name = scope.namer name in
    Id.{
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
    modules = [];
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

let add_module scope given_name exposed =
    let id = make_id scope given_name in
    let modu = Resolved.Module.{
        id; exposed
    } in
    set_resolution scope given_name (fun res -> { res with
        modu = Some modu;
    });
    id

(* TODO: s//add_global_binding *)
let add_local_binding scope exposed =
    let id = scope.namer exposed in
    (* TODO: what when I re-expose stuff? *)
    set_resolution scope exposed (fun res -> { res with
        binding = Some (Resolved.Binding.Local id);
    });
    id

let add_global_binding scope exposed scheme =
    let id = make_id scope exposed in
    (* TODO: what when I re-expose stuff? *)
    set_resolution scope exposed (fun res -> { res with
        binding = Some (Resolved.Binding.Global {id; scheme})
    });
    id

let set_global_binding scope exposed scheme =
    set_resolution scope exposed (fun res -> { res with
        binding = match res.binding with
            | None -> (raise (Invalid_argument "binding not found"))
            | Some (Resolved.Binding.Local _) -> (raise (Invalid_argument "existing binding is local"))
            | Some (Resolved.Binding.Global g) -> 
                Some (Resolved.Binding.Global {g with scheme})
    })

let add_typedef scope name params def =
    let id = make_id scope name in
    (* TODO: what when I re-expose stuff? *)
    set_resolution scope name (fun res -> { res with
        typedef = Some Resolved.Typedef.{
            id;
            params;
            def
        }
    });
    id

let import scope name ext = 
    let replace n old = if Option.is_some n then n else old in
    set_resolution scope name Resolved.Module.(fun existing -> {
        modu = replace ext.modu existing.modu;
        typedef = replace ext.typedef existing.typedef;
        binding = replace ext.binding existing.binding;
    })
    

let lookup_exposed exposed path = 
    let rec lookup_exposed' exposed = function
        | [] -> raise (Invalid_argument "no path is provided")
        | name :: [] -> 
            Map.find exposed name
            |> Option.map ~f:(fun m -> 
                m, Typed_common.Qualified.{
                    name = Resolved_name.make name None; (* found entity contains multiple namespaces: caller must pick which one was actually looked for*)
                    path = [];
                }
            )
        | name :: rest -> (
            Map.find exposed name
            |> Util.Option.flat_map (fun Resolved.Module.{modu; _} -> modu)
            |> Util.Option.flat_map (fun modu ->
                lookup_exposed' Resolved.Module.(modu.exposed) rest
                |> Option.map ~f:(fun (sym, qual) -> 
                    sym, Qualified.{qual with path = (Resolved_name.make name (Some(modu.id))) :: qual.path}
                ) 
            )
        )
    in lookup_exposed' exposed path

let mprint m = Map.iteri m ~f: (fun ~key ~data -> match Resolved.Module.(data.binding) with 
    | None -> ()
    | Some m -> Common.log Resolved.Binding.[ key; "="; 
        match m with 
        | Local loc -> loc 
        | Global g -> g.id.name
    ]
)

let lookup scope qual = 
    let rec lookup' scope = function
        | [] -> raise (Invalid_argument "expected qualified name, got empty one")
        | name :: rest ->
            match Map.find scope.names name with
            | Some exposed ->
                (match rest with
                    | [] -> Some (exposed, Qualified.make (Resolved_name.make name None) [])
                    | rest -> exposed.modu 
                        |> Util.Option.flat_map (fun m ->
                            lookup_exposed Resolved.Module.(m.exposed) rest
                            |> Option.map ~f: (fun (sym, path) -> 
                                    (sym, Typed_common.Qualified.append (Resolved_name.make name (Some m.id)) path))))
            | None -> 
                match scope.parent with 
                | Some ps -> lookup' ps (name :: rest)
                | None -> None
    in 
    lookup' scope (Qualified.given_names qual)