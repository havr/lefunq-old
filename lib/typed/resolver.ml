open Base
open Common
open Typed_common

module Scope = struct 
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
        | [] -> Some Symbol.Module.{modu = Some modu; typedef = None; binding = None}
        | name :: [] -> Map.find Symbol.Module.(modu.exposed) name
        | name :: rest ->
            (match Map.find modu.exposed name with
            | None -> None
            | Some exposed -> (match Symbol.Module.(exposed.modu) with 
                | None -> None
                | Some m -> lookup_modu m rest
            ))

    (* let lookup_resol modu = function
        | [] -> Some modu
        | name :: rest ->
            (match Map.find Symbol.Module.(modu.exposed) name with
            | None -> None
            | Some {modu = None; _} -> None
            | Some {modu = Some m; _} -> lookup_modu m (name :: rest)) *)

    type t = {
        parent: t option;
        source: string;
        modules: string list;
        namer: string -> string;
        mutable names: Symbol.Module.exposed StringMap.t;
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
        | None -> Symbol.Module.{modu = None; typedef = None; binding = None}
        | Some value -> value
        in scope.names <- Map.set scope.names ~key: name ~data: (fn existing)
        
        (* in let new' = {
            modu = replace ext.modu existing.modu;
            typedef = replace ext.typedef existing.typedef;
            binding = replace ext.binding existing.binding;
        } in scope.names <- Map.set scope.names ~key: name ~data: new' *)

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
        modules = scope.modules;
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

    let set_binding_scheme scope given_name scheme =
        scope.names <- Map.change scope.names given_name ~f:(function
            | None -> raise (Invalid_argument ("%s not found " %% [given_name]))
            | Some r -> (match r.binding with
                | None -> raise (Invalid_argument ("binding %s not found" %% [given_name]))
                | Some b -> Some {r with binding = Some {b with scheme}}
            )
        )

    let add_module scope given_name exposed =
        let id = make_id scope given_name in
        let modu = Symbol.Module.{
            id; exposed
        } in
        set_resolution scope given_name (fun res -> { res with
            modu = Some modu;
        });
        id

    let add_binding scope exposed scheme =
        let id = make_id scope exposed in
        (* TODO: what when I re-expose stuff? *)
        set_resolution scope exposed (fun res -> { res with
            binding = Some Symbol.Binding.{
                id;
                scheme
            }
        });
        id

    let add_typedef scope name params def =
        let id = make_id scope name in
        (* TODO: what when I re-expose stuff? *)
        set_resolution scope name (fun res -> { res with
            typedef = Some Symbol.Typedef.{
                id;
                params;
                def
            }
        });
        id

    let implant_binding scope exposed scheme =
        let id = Id.make "" [] exposed in
        set_resolution scope exposed (fun res -> { res with
            binding = Some Symbol.Binding.{
                id;
                scheme
            }
        });
        id

    let import scope name ext = 
        let replace n old = if Option.is_some n then n else old in
        set_resolution scope name Symbol.Module.(fun existing -> {
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
                        name = Resolved.make name None; (* found entity contains multiple namespaces: caller must pick which one was actually looked for*)
                        path = [];
                    }
                )
            | name :: rest -> (
                Map.find exposed name
                |> Util.Option.flat_map (fun Symbol.Module.{modu; _} -> modu)
                |> Util.Option.flat_map (fun modu ->
                    lookup_exposed' Symbol.Module.(modu.exposed) rest
                    |> Option.map ~f:(fun (sym, qual) -> 
                        sym, Qualified.{qual with path = (Resolved.make name (Some(modu.id))) :: qual.path}
                    ) 
                )
            )
        in lookup_exposed' exposed path

    let mprint m = Map.iteri m ~f: (fun ~key ~data -> match Symbol.Module.(data.binding) with 
        | None -> ()
        | Some m -> Common.log [key; "="; m.id.name]
    )

    let lookup scope qual = 
        let rec lookup' scope = function
            | [] -> raise (Invalid_argument "expected qualified name, got empty one")
            | name :: rest ->
                match Map.find scope.names name with
                | Some exposed ->
                    (match rest with
                        | [] -> Some (exposed, Qualified.make (Resolved.make name None) [])
                        | rest -> exposed.modu 
                            |> Util.Option.flat_map (fun m ->
                                lookup_exposed Symbol.Module.(m.exposed) rest
                                |> Option.map ~f: (fun (sym, path) -> 
                                     (sym, Typed_common.Qualified.append (Resolved.make name (Some m.id)) path))))
                | None -> 
                    match scope.parent with 
                    | Some ps -> lookup' ps (name :: rest)
                    | None -> None
        in 
        lookup' scope (Qualified.given_names qual)
end