open Base
open Util
open Node
(* open Error *)

let rec type_param tempvar typ shape = 
    (* s/valid_tuple_type/something better*)
    let match_tuple valid_tuple_type tuple_shape = 
        List.zip_exn valid_tuple_type tuple_shape
        |> List.map ~f: (fun (typ, param) -> 
            match Param.(param.type') with
            | Type.Unknown -> type_param tempvar typ param.shape
            | t -> begin 
                if Type.equals t param.type' then
                    type_param tempvar typ param.shape
                else
                    type_param tempvar t param.shape
                    (* todo: error about mismatch *)
            end
        ) 
    in

    (* let arg_shape = Param.(arg.shape) in
    let arg_typ = Param.(arg.typ) in *)
    match (shape, typ) with
    | Param.Unit, Type.Unknown ->
        Param.{shape = Param.Unit; type' = Base_types.unit }
    | Param.Unit, t ->
        (* TODO: throw an error, this is quite unusual *)
        Param.{shape = Param.Unit; type' = t}
    | Param.Name n, Type.Unknown -> 
        Param.{shape = Param.Name n; type' = tempvar() }
    | Param.Tuple n, Type.Unknown -> begin
        let typed = List.map n ~f:(fun (Param.{type'; shape}) -> type_param tempvar type' shape) in
        let type' = Type.Tuple (List.map typed ~f:(fun entry -> entry.type')) in
        Param.{shape = Param.Tuple typed; type' = type'}
    end
    | Param.Name _, _ -> Param.{shape = shape; type' = typ} (* TODO: check param name *)
    | Param.Tuple tuple_shape, Type.Tuple tuple_type ->
        if List.length tuple_shape < List.length tuple_type then begin
            (* type error: (a, b) != (Int, Int, Int) *)
            let valid_tuple_type = List.take tuple_type (List.length tuple_shape) in
            (* TODO: s/tuple_shape/tuple_params *)
            let typed = match_tuple valid_tuple_type tuple_shape in
            Param.{
                shape = Param.Tuple typed;
                type' = Type.Tuple tuple_type
            }
        end else if List.length tuple_shape > List.length tuple_type then begin
            (* TODO: dont' pass namer as a dep, make sub func *)
            (* TODO: order of params everywhere *)
            (* type error: (a, b, c) != (Int, Int) *)
            let valid = match_tuple tuple_type (List.take tuple_shape (List.length tuple_type)) in
            let invalid = List.drop tuple_shape (List.length tuple_type) 
                |> List.map ~f: (fun param -> type_param tempvar Param.(param.type') param.shape) in
            Param.{
                shape = Param.Tuple (valid @ invalid);
                type' = Type.Tuple tuple_type
            }
        end else begin 
            let params = match_tuple tuple_type tuple_shape in
            Param.{
                shape = Param.Tuple params;
                type' = Type.Tuple tuple_type
            }
        end
    | Param.Tuple _, _ -> 
        raise Common.TODO
        (* maybe other_type is actually a tuple *)
        (* but now just throw an error *)

let type_params tempvar params = 
    List.map params ~f:(fun param -> 
        type_param tempvar Param.(param.type') Param.(param.shape)
    )

open Resolver

type context = {
    scope: Scope.t;
    mutable errors: Erro.t list;
    tempvar: tempvar_gen;
}

let make_context scope = {
    scope;
    tempvar = make_tempvar_gen "p";
    errors = [];
}

let add_errors ~ctx errors = List.iter errors ~f: (fun error -> ctx.errors <- ctx.errors @ [error])

let local_scope ctx = {
    ctx with scope = Scope.sub_local ctx.scope
}

let ident ctx ident = 
    match Scope.lookup ctx.scope Node.Ident.(ident.given_name) with
    | Some {binding = Some binding; _} -> 
        { ident with
            scheme = Some binding.scheme;
            resolved = Some (binding.exposed)
        }
    | Some {binding = None; _} -> 
    (* TODO: rly *)
        add_errors ~ctx [Erro.UndeclaredIdentifier {given_name = ident.given_name; range = ident.range}];
        ident
    | None -> 
        add_errors ~ctx [Erro.UndeclaredIdentifier {given_name = ident.given_name; range = ident.range}];
        ident

let rec binding ctx node = 
    match Let.(node.is_rec) with
    | true ->
        let scheme = Type.make_scheme ["t"] (Type.Var "t") in
        let id = Scope.add_binding ctx.scope Let.(node.given_name) scheme
        in { node with 
            scheme = Some scheme;
            block = block ctx Let.(node.block); 
            scope_name = id.name
        }
    | false ->
        let scheme = Type.make_scheme [] Type.Unknown in
        let b = block ctx Let.(node.block) in
        let id = Scope.add_binding ctx.scope Let.(node.given_name) scheme in
        { node with 
            scheme = Some scheme;
            block = b;
            scope_name = id.name
        }

and block ctx n =
    let stmts = List.map Block.(n.stmts) ~f:(function 
        | Stmt.Expr n -> 
            Stmt.Expr (expr ctx n)
        | Stmt.Let n -> 
            let result = (binding ctx n) in
            Stmt.Let result
        | Stmt.Block 
            _ -> raise Common.TODO (* Disallow block inside blocks *)
    ) in {n with stmts = stmts}
and expr ctx = function
| Expr.Value v -> Expr.Value v
| Expr.Ident n -> Expr.Ident (ident ctx n)
| Expr.Apply n -> Expr.Apply (apply ctx n)
| Expr.Lambda n -> Expr.Lambda (lambda ctx n)
| Expr.Cond n -> Expr.Cond (cond ctx n )
| Expr.Tuple _ -> raise Common.TODO
and apply ctx n = 
    let fn_result = expr ctx Apply.(n.fn) in
    let arg_results = List.map ~f: (expr ctx) n.args in
    { n with fn = fn_result; args = arg_results }
and cond ctx n = 
    let cases = List.map n.cases ~f:(fun {if_; then_} ->
        Cond.{
            if_ = block (local_scope ctx) if_;
            then_ = block (local_scope ctx) then_;
        }
    ) in
    let else_ = Cond.(n.else_) |> Option.map ~f: (block (local_scope ctx)) in
    { n with cases = cases; else_ = else_ }
and lambda ctx n = 
    (* TODO: resolve type names *)
    let rec extract_names param = match Param.(param.shape) with
    | Param.Name n -> [n, Type.make_scheme [] param.type']
    | Param.Tuple t -> List.map t ~f:extract_names |> List.concat
    | Param.Unit -> []
    in 
    let report_duplicates param_names =
        let as_map = List.fold param_names ~init: (Map.empty(module String)) ~f:(fun map (name, typ) ->
            if Map.mem map name then (* throw an error *) map
            else (Map.add_exn map ~key: name ~data: typ)) in
        Map.to_alist as_map
    in 
    
    let typed_params = type_params ctx.tempvar Lambda.(n.params) in
    let type_names = report_duplicates (List.map typed_params ~f:extract_names |> List.concat) in
    let sub = Resolver.Scope.sub_local ctx.scope in
    List.iter type_names
        ~f: (fun (name, scheme) -> 
            ignore @@ Scope.add_binding sub name scheme);
    { n with params = typed_params; block = block {ctx with scope = sub} n.block }

type resolve_source_error = 
    | CyclicDependency of string list
    | SourceNotFound 
    | SystemError
    | SourceError

let import resolve_source resolver node = 
    let errors = ref [] in
    let rec lookup_path resol = function
    | [] -> Ok resol
    | name :: rest ->
      match Resolver.Scope.lookup_resol resol [Common.Span.(name.value)] with
      | None -> Error (Erro.SourceSymbolNotFound { source = Node.Import.(node.source); symbol = name })
      | Some r -> lookup_path r rest
    in
    let node' = match resolve_source Node.Import.(node.source.value) with
    (* TODO: errors when shadowing imports *)
    | Error (CyclicDependency list) -> 
        errors := (Erro.CyclicDependency {list; caused_by = node.source}) :: !errors;
        node
    | Error (SourceNotFound) -> 
        errors := (Erro.SourceNotFound {source = node.source}) :: !errors;
        node
    | Error (SourceError) ->
        errors := (Erro.SourceCompileError {source = node.source}) :: !errors;
        node
    | Error (SystemError) ->
        errors := (Erro.SourceSystemError {source = node.source}) :: !errors;
        node
    | Ok (resolved_source, source_scope) ->
        let names = List.map node.names ~f:(fun {name; path; _} -> 
            match lookup_path source_scope path with
            | Error e -> 
                errors := e :: !errors;
                Node.Import.{name; path; resolved = None}
            | Ok r ->
                Resolver.Scope.import resolver name.value r;
                Node.Import.{name; path; resolved = Some r}
        ) in {node with names; resolved_source}
    in (node', !errors)
