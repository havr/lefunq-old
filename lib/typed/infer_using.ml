open Base
open Common
open Typed_common
open Node

type resolve_source_error = 
    | CyclicDependency of string list
    | SourceNotFound 
    | SystemError
    | SourceError

type resolve_source_fn = string -> (string * Resolved.Module.exposed StringMap.t, resolve_source_error) Result.t

let using resolve_source resolver node = 
    let errors = ref [] in

    let lookup_exposed rootmod path = 
        let rec loop' names = function
        | [] -> raise (Invalid_argument "no path is provided")
        | namespan :: [] -> (match Map.find names Common.Span.(namespan.value) with 
            | None -> Error (Errors.UndeclaredIdentifier { range = namespan.range; given_name = namespan.value})
            (* | None -> Error (Errors.SourceSymbolNotFound { source; symbol = namespan}) *)
            | Some m -> Ok m
        )
        | moduspan :: rest -> (
            match Map.find names Common.Span.(moduspan.value) with
            | None -> Error (Errors.UndeclaredIdentifier { range = moduspan.range; given_name = moduspan.value})
            (* | None ->  Error (Errors.SourceSymbolNotFound { source; symbol = moduspan}) *)
            | Some exposed -> (match Resolved.Module.(exposed.modu) with
            (* TODO: Is not a module error *)
                | None -> Error (Errors.UndeclaredIdentifier { range = moduspan.range; given_name = moduspan.value})
                (* | None -> Error (Errors.SourceSymbolNotFound { source; symbol = moduspan}) *)
                | Some modu -> loop' Resolved.Module.(modu.exposed) rest
            )
        ) in match path with
        | [] -> Ok (Resolved.Module.{modu = Some rootmod; typedef = None; binding = None})
        | path -> loop' (rootmod.exposed) path
    in
    let mr = (match Using.(node.root) with
    | Using.Local name -> 
        (match (Scope.lookup resolver (Qualified.from_string name.value)) with 
            | Some (m, _) -> (match m.modu with 
                | Some modu -> Ok (modu, node)
                | None -> Error (Errors.NotModule {name})
            )
            | None ->
                Error (Errors.UndeclaredIdentifier {range = name.range; given_name = name.value}))
    | Using.Source {name; _} -> 
        (match (resolve_source name.value) with 
            | Ok (resolved_source, source_scope) ->
                Ok (Resolved.Module.{
                    id = Id.make resolved_source [] "";
                    exposed = source_scope;
                }, {node with root = Using.Source {name; resolved = resolved_source}})
            | Error (CyclicDependency list) -> 
                Error (Errors.CyclicDependency {list; caused_by = name})
            | Error (SourceNotFound) -> 
                Error (Errors.SourceNotFound {source = name}) 
            | Error (SourceError) ->
                Error (Errors.SourceCompileError {source = name});
            | Error (SystemError) ->
                Error (Errors.SourceSystemError {source = name});)
    ) in
    let node' = (match mr with
        | Error e -> 
            errors := e :: !errors;
            node;
        | Ok (m, node) ->
        let resolved = List.map node.names ~f: (fun (imp, path) ->
            match lookup_exposed m path with
            | Error e ->
                errors := e :: !errors;
                Map.empty(module String)
            | Ok exposed ->
                (match imp with 
                  | Using.Only n -> Map.singleton (module String) n.value exposed
                  | Using.Wildcard -> (
                    match exposed.modu with
                    | Some modu -> modu.exposed
                        (* |> List.map ~f:(fun (k, v) -> (k, Resolved.Module.(v.exposed))) *)
                    | None -> 
                        let name = List.last_exn path in
                        errors := (Errors.NotModule {name}) :: !errors;
                        Map.empty(module String)
                ))
        ) in
        List.iter resolved ~f:(fun names ->
            Map.iteri names ~f: (fun ~key ~data ->
                Scope.import resolver key data;
            );
        );
        node
    )
    in (node', !errors)