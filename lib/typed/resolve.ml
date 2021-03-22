open Base
open Type_util
open Common
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

let resolve_typ typ = 
    let rec resolve = function
    (* TODO: Pay attention to vars up scope *)
    | Type.Unit -> Type.Unit
    | Type.Var name -> Type.Var name
    | Type.Simple name -> Type.Simple name (* TODO: proper name resolution *)
    | Type.Lambda (h, t) ->
        Type.Lambda (resolve h, resolve t)
    | Type.Tuple elems ->
        Type.Tuple (List.map elems ~f: resolve)
    | Type.Unknown -> Type.Unknown
    in
    let result = resolve typ in
    (* TODO: add errors *)
    result

let init_last list = 
    let rec loop result = function
        | [] -> raise (Invalid_argument "list is empty")
        | last :: [] -> (result, last)
        | n :: m -> loop (result @ [n]) m 
    in loop [] list

let apply_sig sigt params =
    let unrolled = match sigt with 
        | Type.Lambda lam -> Type.Lambda.unroll lam 
        | t -> [t]
    in
    let param_types = List.map params ~f: (fun p -> Param.(p.type')) in
    let rec apply_sig_type = function
    | (s :: srest, p :: prest) -> 
        Common.log ["*"; Type.to_string s; Type.to_string p];
        (match p with
            | Type.Unknown -> 
                s (* TODO: what about resolution? resolve sig first? *)
            | _ ->
                (*  TODO: match two types and resolve type vars here *)
                raise Common.TODO
        ) :: (apply_sig_type (srest, prest))
    | ([], []) -> []
    | ([], _) ->
        raise Common.TODO (* signature doesn't fit the given number of parameters *)
    | (srest, []) ->
        srest
    in
    let sigt_params, result = init_last @@ apply_sig_type (unrolled, param_types @ [Type.Unknown]) in
    (List.zip_exn sigt_params params 
        |> List.map ~f:(fun (typ, param) -> Param.{param with type' = typ})), result


let type_params tempvar params = 
    (* let resolved_params = List.map resolve_typ ~f: params in
    let resolved_sigt = sigt 
        |> Option.map ~f: (fun sigt -> 
            resolve_typ (sigt @@ [tempvar()])
        )
        
    in *)


    List.map params ~f:(fun param -> 
        type_param tempvar Param.(param.type') Param.(param.shape)
    )

open Resolver

type context = {
    source: string;
    scope: Scope.t;
    mutable errors: Erro.t list;
    tempvar: tempvar_gen;
}

let make_context ?(source="") scope = {
    source;
    scope;
    tempvar = make_tempvar_gen "p";
    errors = [];
}

let add_errors ~ctx errors = List.iter errors ~f: (fun error -> ctx.errors <- ctx.errors @ [error])

let local_scope ctx = {
    ctx with scope = Scope.sub_local ctx.scope
}

let ident ctx ident = 
    let resolution = Node.Ident.(ident.resolution) @ [ident.resolved] in
    match Scope.lookup ctx.scope resolution with
    | Some ({binding = Some binding; _}, resolved) -> 
        (* TODO: continue here: local bindings use their resolved, not exposed values *)
        let resolved, resolution = resolved @ [Symbol.Resolved.make ident.resolved.given (Some binding.id)]
            |> Util.Lists.last_rest in
        { ident with
            scheme = Some binding.scheme;
            (* TODO: remove the if *)
            resolved; resolution;
        }
    | Some ({binding = None; _}, _) -> 
    (* TODO: rly *)
        add_errors ~ctx [Erro.UndeclaredIdentifier {given_name = ident.resolved.given; range = ident.range}];
        ident
    | None -> 
        add_errors ~ctx [Erro.UndeclaredIdentifier {given_name = ident.resolved.given; range = ident.range}];
        ident

let flatten list = 
    let rec flatten' result = function
        | [] -> result
        | a :: rest -> flatten' (result @ a) rest
    in flatten' [] list

let sub_ctx ctx scope = {ctx with scope = scope ctx.scope}

let rec local_binding ctx node = 
    binding ~sub_scope: (Resolver.Scope.sub_local) ctx node

and binding ~sub_scope ctx node = 
    let params, result = match Let.(node.sigt) with 
        | None -> 
            let map_param param = match Param.(param.type') with
            | Type.Unknown -> {param with type' = ctx.tempvar()}
            | _ -> param
            in (node.params |> List.map ~f:map_param), (ctx.tempvar())
        | Some sigt -> apply_sig sigt node.params
    in
    let ctx' = sub_ctx ctx sub_scope in
    let params = resolve_params ctx' params in
    let param_types = List.map params 
        ~f:(fun param -> Param.(param.type')) @ [result]
    in
    let free_vars = List.map param_types ~f:Type.free_vars 
        |> Set.union_list(module String) 
        |> Set.to_list in
    let typ = match param_types with
    | only :: [] -> only
    | multiple -> Type.Lambda.make multiple
    in
    let scheme = Type.make_scheme free_vars typ in
    Option.iter node.sigt ~f: (fun opt -> Common.log ["sigt"; Type.to_string opt]);
    Common.log [node.given_name; Type.scheme_to_string scheme];
    let result = match Let.(node.is_rec) with
    | true ->
        let id = Scope.add_binding ctx.scope Let.(node.given_name) scheme in
        { node with 
            params = params;
            scheme = Some scheme;
            block = block ctx' Let.(node.block); 
            scope_name = id.name;
            result = result
        }
    | false ->
        let b = block ctx' Let.(node.block) in
        ctx.errors <- ctx.errors @ ctx'.errors;
        let id = Scope.add_binding ctx.scope Let.(node.given_name) scheme in
        { node with 
            scheme = Some scheme;
            block = b;
            scope_name = id.name;
            params = params;
            result = result
        }
    in 
        ctx.errors <- ctx.errors @ ctx'.errors;
        result

and block ctx n =
    let stmts = List.map Block.(n.stmts) ~f:(function 
        | Stmt.Expr n -> 
            Stmt.Expr (expr ctx n)
        | Stmt.Let n -> 
            let result = (local_binding ctx n) in
            Stmt.Let result
        | Stmt.Block 
            _ -> raise Common.TODO (* Disallow block inside blocks *)
    ) in {n with stmts = stmts}
and expr ctx = function
| Expr.Value v -> Expr.Value v
| Expr.Ident n -> Expr.Ident (ident ctx n)
| Expr.Apply n -> Expr.Apply (apply ctx n)
| Expr.Lambda n -> Expr.Lambda (lambda ctx n)
| Expr.Li li -> Expr.Li {li with items = List.map ~f: (expr ctx) li.items}
| Expr.Foreign f -> Expr.Foreign f
| Expr.Cond n -> Expr.Cond (cond ctx n )
| Expr.Tuple t -> Expr.Tuple {t with exprs = List.map ~f:(expr ctx) t.exprs}
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
and resolve_params ctx params = 
    (* TODO: report unused idents *)
    (* let rec extract_names param = match Param.(param.shape) with
    | Param.Name n -> [n.given, Type.make_scheme [] param.type']
    | Param.Tuple t -> List.map t ~f:extract_names |> List.concat
    | Param.Unit -> []
    in 
    let report_duplicates param_names =
        let as_map = List.fold param_names ~init: (Map.empty(module String)) ~f:(fun map (name, typ) ->
            if Map.mem map name then (* throw an error *) map
            else (Map.add_exn map ~key: name ~data: typ)) in
        Map.to_alist as_map
    in  *)
    let typed_params = type_params ctx.tempvar params in
    let rec resolve_param param = Param.{ param with shape = match (param.shape) with
            | Name n -> Name {n with resolved = (Scope.add_binding ctx.scope n.given (Type.make_scheme [] param.type')).name}
            | Tuple t -> Tuple (List.map t ~f: resolve_param)
            | Unit -> Unit
        }
    in
    let typed_params = List.map ~f:resolve_param typed_params in
    (* let type_names = report_duplicates (List.map typed_params ~f:extract_names |> List.concat) in *)
    typed_params

and lambda ctx n = 
    (* TODO: resolve type names *)
    let ctx' = sub_ctx ctx (Resolver.Scope.sub_local) in
    let typed_params = resolve_params ctx' Lambda.(n.params) in
    let b = block ctx' n.block in
    ctx.errors <- ctx.errors @ ctx'.errors;
    { n with params = typed_params; block = b}


let toplevel_binding ctx n =
    binding ~sub_scope: (Resolver.Scope.toplevel) ctx n


type resolve_source_error = 
    | CyclicDependency of string list
    | SourceNotFound 
    | SystemError
    | SourceError


let import resolve_source resolver node = 
    let errors = ref [] in

    let rec lookup_exposed names = function
    | [] -> raise (Invalid_argument "no path is provided")
    | namespan :: [] -> (match Map.find names Common.Span.(namespan.value) with 
      | None -> Error (Erro.SourceSymbolNotFound { source = Node.Import.(node.source); symbol = namespan})
      | Some m -> Ok m
    )
    | moduspan :: rest -> (
        match Map.find names Common.Span.(moduspan.value) with
        | None ->  Error (Erro.SourceSymbolNotFound { source = Node.Import.(node.source); symbol = moduspan})
        | Some exposed -> (match Symbol.Module.(exposed.modu) with
        (* TODO: Is not a module error *)
            | None -> Error (Erro.SourceSymbolNotFound { source = Node.Import.(node.source); symbol = moduspan})
            | Some modu -> lookup_exposed Symbol.Module.(modu.exposed) rest
        )
    )
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
            match lookup_exposed source_scope path with
            | Error e -> 
                errors := e :: !errors;
                Node.Import.{name; path; resolved = None}
            | Ok r ->
                Resolver.Scope.import resolver name.value r;
                Node.Import.{name; path; resolved = Some r}
        ) in {node with names; resolved_source}
    in (node', !errors)
