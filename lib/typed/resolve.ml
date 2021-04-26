open Base
open Type_util
open Common
open Typed_common
open Node
open Resolver

module Ctx = struct 
    type context = {
        source: string;
        scope: Scope.t;
        mutable errors: Errors.t list;
        tempvar: tempvar_gen;
    }

    let make ?(source="") scope = {
        source;
        scope;
        tempvar = make_tempvar_gen "_p";
        errors = [];
    }

    let add_errors ~ctx errors = List.iter errors ~f: (fun error -> ctx.errors <- ctx.errors @ [error])

    let local_scope ctx = {
        ctx with scope = Scope.sub_local ctx.scope
    }

    let sub_ctx ctx scope = {ctx with scope = scope ctx.scope}
end

let type_name ~ctx range name = 
    match Scope.lookup Ctx.(ctx.scope) Type.(name.resolved) with
    | Some ({typedef = Some typedef; _}, resolved) -> 
        (* TODO: continue here: local bindings use their resolved, not exposed values *)
        let qual = Qualified.resolve_name (typedef.id) resolved in
        Type.{
            resolved = qual;
            def = typedef.def;
        }
        
    | Some ({typedef = None; _}, _) -> 
    (* TODO: rly *)
        Ctx.add_errors ~ctx [Errors.UndeclaredIdentifier {given_name = Qualified.given name.resolved; range}];
        name
    | None -> 
        Ctx.add_errors ~ctx [Errors.UndeclaredIdentifier {given_name = Qualified.given name.resolved; range}];
        name

let rec type_ident ~ctx = function
    | TypeIdent.Simple {given; name; args} ->
        TypeIdent.Simple {given; args; name = type_name ~ctx (given.range) name}
    | TypeIdent.Lambda {arg; result} ->
        TypeIdent.Lambda {
            arg = TypeIdent.{arg with typ = type_ident ~ctx arg.typ};
            result = type_ident ~ctx result
        }
    | TypeIdent.Var v -> TypeIdent.Var v
    | TypeIdent.Tuple {items} -> TypeIdent.Tuple {items = List.map items ~f:(type_ident ~ctx)}
    | TypeIdent.Unit -> TypeIdent.Unit

let rec type_from_ident = function
    | TypeIdent.Var {given} -> Type.Var given.value
    | TypeIdent.Simple {name; args; _} ->
        Type.Simple (name, List.map args ~f: type_from_ident)
    | TypeIdent.Lambda {arg; result} ->
        let from = match arg.label with
            | "" -> Type.PosParam (type_from_ident arg.typ)
            | label -> 
                Type.NamedParam {is_optional = arg.optional; param_name = label; param_typ = type_from_ident arg.typ}
        in
        Type.Lambda (from, type_from_ident result)
    | TypeIdent.Tuple {items} -> Type.Tuple (List.map items ~f:(type_from_ident))
    | TypeIdent.Unit -> Type.Unit

 module Params = struct
    (* open Node.Param *)
    
    let rec match_pattern ~ctx typ pat = 
        let add_name typ name' =  
            let id = Resolver.Scope.add_binding Ctx.(ctx.scope) Node.Param.(name'.given) (Type.make_scheme [] typ) in
            Param.{name' with scope = id.name}
        in
        let match_tuple typs pats = 
            Util.Lists.map2 typs pats ~f:(fun (typ, pat) -> match_pattern ~ctx typ pat)
        in

        match (typ, pat) with
        | Type.Unknown, Param.Unit ->
            (Base_types.unit, Param.Unit)
        | t, Param.Unit ->
            (* TODO: assert if t is Unit *)
            (t, Param.Unit)

        | Type.Unknown, Param.Name n -> 
            let typ = ctx.tempvar() in
            (typ, Param.Name (add_name typ n))

        | t, Param.Name n -> 
            (t, Param.Name (add_name t n))

        | Type.Unknown, Param.Tuple tup -> 
            let (typs, pats) = 
                List.map tup ~f:(match_pattern ~ctx Type.Unknown) |> List.unzip in
            (Type.Tuple typs, Param.Tuple pats)

        | Type.Tuple typs, Param.Tuple pats ->
            let pat_len = List.length pats in
            let typ_len = List.length typs in
            if pat_len = typ_len then 
                let (typ, pats) = match_tuple typs pats in
                (Type.Tuple typ, Param.Tuple pats)
            else if (pat_len < typ_len) then
                (* TODO: s/typle_pat/typle_params *)
                (* TODO: throw an error error *)
                let (_, pats') = match_tuple (List.take typs pat_len) pats in
                (Type.Tuple typs, Param.Tuple pats')
            else (
                (* TODO: throw an error error *)
                let (_, matched) = match_tuple typs (List.take pats typ_len) in
                let unmatched = List.drop pats typ_len in
                (Type.Tuple typs, Param.Tuple (matched @ unmatched))
            )
        | _, Param.Tuple _ -> 
            raise Common.TODO

    let resolve ~ctx ~expr param = 
        let add_name typ n =  
            let typ' = match typ with
                | Type.Unknown -> Ctx.(ctx.tempvar)()
                | t -> t
            in
            let id = Resolver.Scope.add_binding Ctx.(ctx.scope) Node.Param.(n.given) (Type.make_scheme [] typ') in
            typ', Param.{n with scope = id.name}
        in
        (* TODO: provide proper typename range *)
        let resolved_type_ident = Node.Param.(param.type_ident) 
            |> Option.map ~f: (type_ident ~ctx: ctx)
        in
        let typ = match resolved_type_ident with 
            | Some ti -> type_from_ident ti
            | None -> Type.Unknown
        in
        match param.value with
        | Param.Positional p -> 
            let typ, pattern = match_pattern ~ctx typ p.pattern in
            Param.{
                typ;
                type_ident = resolved_type_ident;
                value = Param.Positional {pattern}
            }
        | Param.Optional p -> 
            let typ, name' = add_name typ p.name in
            Param.{
                typ;
                type_ident = resolved_type_ident;
                value = Param.Optional {
                    name = name';
                    default = Option.map p.default ~f: (fun e -> expr e)
                }
            }
        | Param.Named p -> 
            let typ, name' = add_name typ p.name in
            Param.{
                typ;
                type_ident = resolved_type_ident;
                value = Param.Named {name = name'}
            }
        | Param.Extension p -> 
            let typ, name' = add_name typ p.name in
            Param.{
                typ;
                type_ident = resolved_type_ident;
                value = Param.Extension {name = name'}
            }


    let to_type params = 
        List.map params ~f: Param.(fun {typ; value; _} ->
            match value with
                | Positional _ -> Type.PosParam typ
                | Named {name} -> Type.NamedParam {param_typ = typ; is_optional = false; param_name = name.given}
                | Optional {name; _} -> Type.NamedParam {param_typ = typ; is_optional = true; param_name = name.given}
                | Extension _ -> (raise Common.TODO) (* flatmap struct here *)
    )

    let to_lambda params result = 
        Type.Lambda.make (to_type params) result
end

let resolve_typ typ = 
    let rec resolve = function
    (* TODO: Pay attention to vars up scope *)
    | Type.Invalid -> Type.Invalid
    | Type.Unit -> Type.Unit
    | Type.Var name -> Type.Var name
    | Type.Simple name -> Type.Simple name (* TODO: proper name resolution *)
    | Type.Lambda (h, t) ->
        let h' = (match h with
            | Type.PosParam p -> Type.PosParam (resolve p)
            | Type.NamedParam p -> Type.NamedParam {p with param_typ = resolve p.param_typ}
            | Type.NamedBlock b -> Type.NamedBlock (List.map b
                ~f:(fun np -> 
                    {np with param_typ = resolve np.param_typ}))) in
        Type.Lambda (h', resolve t)
    | Type.Tuple elems ->
        Type.Tuple (List.map elems ~f: resolve)
    | Type.Unknown -> Type.Unknown
    in
    let result = resolve typ in
    (* TODO: add errors *)
    result

let instantiate_scheme ~ctx scheme =
    let new_substs = List.fold 
        Type.(scheme.constr) 
        ~init: Subst.empty
        ~f: (fun substs var_name ->
            Subst.add substs var_name Ctx.(ctx.tempvar())
    ) in 
    Subst.apply new_substs scheme.typ

let foreign ~ctx f = 
    let ti = type_ident ~ctx Foreign.(f.type_ident) in
    let typ = type_from_ident ti in
    let constr = Type.free_vars typ |> Set.to_list in
    let typ' = instantiate_scheme ~ctx (Type.make_scheme constr typ) in
    Node.Foreign.{
        f with typ = typ'
    }

let ident ctx id = 
    let given = Qualified.given Ident.(id.qual) in
    match Scope.lookup Ctx.(ctx.scope) Ident.(id.qual) with
    | Some ({binding = Some binding; _}, resolved) -> 
        (* TODO: continue here: local bindings use their resolved, not exposed values *)
        let qual = Qualified.resolve_name (binding.id) resolved in
        Node.Ident.{ id with
            typ = instantiate_scheme ~ctx binding.scheme; (* TODO: should I instantiate scheme here*)
            qual
        }
        
    | Some ({binding = None; _}, _) -> 
    (* TODO: rly *)
        Ctx.add_errors ~ctx [Errors.UndeclaredIdentifier {given_name = given; range = id.range}];
        id
    | None -> 
        Ctx.add_errors ~ctx [Errors.UndeclaredIdentifier {given_name = given; range = id.range}];
        id



let rec local_binding ctx node = 
    binding ~sub_scope: (Resolver.Scope.sub_local) ctx node

and binding ~sub_scope root_ctx node = 
    (* let params, result = Let.(node.params |> List.map ~f:(Params.resolve ~tempvar: Ctx.(ctx.tempvar) ~scope: Ctx.(ctx.scope))), (ctx.tempvar()) in *)
    let ctx' = Ctx.sub_ctx root_ctx sub_scope in
    (* let default_ctx = Ctx.sub_ctx root_ctx (Resolver.Scope.sub_local) in *)
    let params = List.map ~f:(Params.resolve ~expr: (expr ctx') ~ctx: ctx') Let.(node.params) in
    let lambda_params = Params.to_type params in
    let result = (root_ctx.tempvar()) in
    let lambda_typ = match lambda_params with
    | [] -> result
    | multiple -> Type.Lambda.make multiple result 
    in
    let free_vars = Type.free_vars (lambda_typ) in
    let scheme = Type.make_scheme (Set.to_list free_vars) lambda_typ in
    let result = match Let.(node.is_rec) with
    | true ->
        let id = Scope.add_binding root_ctx.scope Let.(node.given_name) scheme in
        { node with 
            params = params;
            scheme = Some scheme;
            block = block ctx' Let.(node.block); 
            scope_name = id.name;
            result = result
        }
    | false ->
        let b = block ctx' Let.(node.block) in
        root_ctx.errors <- root_ctx.errors @ ctx'.errors;
        let id = Scope.add_binding root_ctx.scope Let.(node.given_name) scheme in
        { node with 
            scheme = Some scheme;
            block = b;
            scope_name = id.name;
            params = params;
            result = result
        }
    in 
        root_ctx.errors <- root_ctx.errors @ ctx'.errors;
        result

and matc ~ctx n =
    let rec resolve_pattern ~ctx = function
        | Match.Unit -> Match.Unit
        | Match.Any -> Match.Any
        | Match.Tuple tup -> Match.Tuple (List.map tup ~f:(resolve_pattern ~ctx))
        | Match.List li -> Match.List { 
            items = List.map li.items ~f:(resolve_pattern ~ctx);
            rest = Option.map li.rest ~f:(resolve_pattern ~ctx);
            item_typ = Ctx.(ctx.tempvar)()
        }
        | Match.Str s -> Match.Str s
        | Match.Int i -> Match.Int i
        | Match.Param p -> (
            let typ = ctx.tempvar() in
            let id = Scope.add_binding ctx.scope p.given_name (Type.make_scheme [] typ) in
            Match.Param {p with scope_name = id.name; typ}
        )
    in
    let e = expr ctx Match.(n.expr) in
    let cases = List.map n.cases ~f: (fun case ->
        let ctx = Ctx.sub_ctx ctx Resolver.Scope.sub_local in
        let pattern = resolve_pattern ~ctx case.pattern in
        let stmts = block_stmts ~ctx case.stmts in
        Match.{stmts; pattern}
    ) in Match.{n with cases; expr = e}

and block_stmts ~ctx stmts = List.map stmts ~f:(function 
    | Stmt.Expr n -> 
        Stmt.Expr (expr ctx n)
    | Stmt.Let n -> 
        Stmt.Let (local_binding ctx n)
    | Stmt.Block 
        _ -> raise Common.TODO (* Disallow block inside blocks *)
)

and block ctx n = Block.{n with stmts = block_stmts ~ctx n.stmts}
and li ~ctx n = Li.{n with items = List.map ~f: (expr ctx) n.items}
and tuple ~ctx n = Tuple.{n with exprs = List.map ~f: (expr ctx) n.exprs}
and expr ctx = function
| Expr.Value v -> Expr.Value v
| Expr.Ident n -> Expr.Ident (ident ctx n)
| Expr.Apply n -> Expr.Apply (apply ctx n)
| Expr.Lambda n -> Expr.Lambda (lambda ctx n)
| Expr.Li n -> Expr.Li (li ~ctx n)
| Expr.Foreign f -> Expr.Foreign (foreign ~ctx f)
| Expr.Cond n -> Expr.Cond (cond ctx n )
| Expr.Tuple t -> Expr.Tuple (tuple ~ctx t)
| Expr.Match m -> Expr.Match (matc ~ctx m)
and apply ctx n = Apply.{n with 
        fn = expr ctx n.fn; 
        args = List.map ~f: (arg ctx) n.args; typ = ctx.tempvar() 
    }
and arg ctx = function
    | Apply.PosArg {expr = e} -> Apply.PosArg {expr = expr ctx e}
    | Apply.NameArg {name; expr = e} -> Apply.NameArg {name; expr = expr ctx e}

and cond ctx n = 
    let cases = List.map Cond.(n.cases) ~f:(fun {if_; then_} ->
        Cond.{ if_ = block (Ctx.local_scope ctx) if_; then_ = block (Ctx.local_scope ctx) then_ }
    ) in
    let else_ = Cond.(n.else_) |> Option.map ~f: (block (Ctx.local_scope ctx)) in
    Cond.{ n with cases = cases; else_ = else_; typ = ctx.tempvar() }
and lambda ctx n = 
    let ctx' = Ctx.sub_ctx ctx (Resolver.Scope.sub_local) in
    (* TODO: refactor the way default expr is being passed *)
    let default_ctx = Ctx.sub_ctx ctx (Resolver.Scope.sub_local) in
    let typed_params = List.map ~f:(Params.resolve ~expr: (expr default_ctx) ~ctx:ctx') Lambda.(n.params) in
    let params_type = Params.to_lambda typed_params (ctx.tempvar()) in
    let b = block ctx' n.block in
    ctx.errors <- ctx.errors @ ctx'.errors;
    Lambda.{ n with params = typed_params; block = b; typ = params_type}


let toplevel_binding ctx n =
    binding ~sub_scope: (Resolver.Scope.toplevel) ctx n

type resolve_source_error = 
    | CyclicDependency of string list
    | SourceNotFound 
    | SystemError
    | SourceError

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
            | Some exposed -> (match Symbol.Module.(exposed.modu) with
            (* TODO: Is not a module error *)
                | None -> Error (Errors.UndeclaredIdentifier { range = moduspan.range; given_name = moduspan.value})
                (* | None -> Error (Errors.SourceSymbolNotFound { source; symbol = moduspan}) *)
                | Some modu -> loop' Symbol.Module.(modu.exposed) rest
            )
        ) in match path with
        | [] -> Ok (Symbol.Module.{modu = Some rootmod; typedef = None; binding = None})
        | path -> loop' (rootmod.exposed) path
    in
    let mr = (match Using.(node.root) with
    | Using.Local name -> 
        (match (Resolver.Scope.lookup resolver (Qualified.from_string name.value)) with 
            | Some (m, _) -> (match m.modu with 
                | Some modu -> Ok (modu, node)
                | None -> Error (Errors.NotModule {name})
            )
            | None ->
                Error (Errors.UndeclaredIdentifier {range = name.range; given_name = name.value}))
    | Using.Source {name; _} -> 
        (match (resolve_source name.value) with 
            | Ok (resolved_source, source_scope) ->
                Ok (Symbol.Module.{
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
        let resolved = Util.Lists.flat_map node.names ~f: (fun (imp, path) ->
            match lookup_exposed m path with
            | Error e ->
                errors := e :: !errors;
                []
            | Ok exposed ->
                (match imp with 
                  | Using.Only n -> [n.value, exposed]
                  | Using.Wildcard -> (
                    match exposed.modu with
                    | Some modu -> 
                        Map.to_alist modu.exposed
                        (* |> List.map ~f:(fun (k, v) -> (k, Symbol.Module.(v.exposed))) *)
                    | None -> 
                        let name = List.last_exn path in
                        errors := (Errors.NotModule {name}) :: !errors;
                        [];
                ))
        ) in
        List.iter resolved ~f: (fun (name, r) ->
            Resolver.Scope.import resolver name r;
        );
        node
    )
    in (node', !errors)
