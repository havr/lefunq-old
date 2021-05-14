(* open Base
open Type_util
open Typed_common
open Node
open Resolver

module ErrorSink = struct 
    type t = {
        mutable list: Errors.t list;
    }

    let make = {list = []}
    let put sink errors = sink.list <- errors @ sink.list 
end

module Ctx = struct 
    type context = {
        source: string;
        scope: Scope.t;
        resolve_source: Resolve_using.resolve_source_fn;
        errors: ErrorSink.t;
        tempvar: tempvar_gen;
    }

    let make ?(source="") ~resolve_source scope = {
        source;
        scope;
        resolve_source;
        tempvar = make_tempvar_gen "_p";
        errors = ErrorSink.make;
    }

    let add_errors ~ctx errors = ErrorSink.put ctx.errors errors
        (* Common.log [Common.stacktrace ()];
        List.iter errors ~f: (fun error -> Common.log [Errors.to_string error]);
        Common.log [];
        List.iter errors ~f: (fun error -> ctx.errors <- ctx.errors @ [error]) *)

    let local_scope ctx = {
        ctx with scope = Scope.sub_local ctx.scope
    }

    let sub_ctx ctx scope = {ctx with scope = scope ctx.scope }
end
*)

 (* module Params = struct
    let destruct ~ctx sh = 
        let rec destruct' = function
        | Node.Destruct.Name n ->
            let typ = Ctx.(ctx.tempvar()) in
            let id = Resolver.Scope.add_local_binding Ctx.(ctx.scope) n.given in
            let node' = Node.Destruct.Name Node.Destruct.{n with scope = id; typ} in
            (node', typ)
        | Node.Destruct.Unit ->
            (Node.Destruct.Unit, Type.Unit)
        | Node.Destruct.Tuple tup -> 
            let nodes, typs = List.map tup ~f: destruct'
            |> List.unzip in
            (Node.Destruct.Tuple nodes), Type.Tuple (typs)
        in destruct' sh

    let resolve ~ctx ~expr param = 
        match Param.(param.value) with
        | Param.Positional p -> 
            let shape, typ = destruct ~ctx p.shape in
            Param.{ typ; value = Param.Positional {shape} }
        | Param.Optional p -> 
            let default = Option.map p.default ~f: (fun e -> expr e) in
            let name, typ = add_name (match p.alias with 
                | None -> p.name
                | Some alias -> Param.{given = alias; scope = ""}
            ) in
            Param.{ typ; value = Param.Optional { p with default; name;
                }
            }
        | Param.Named n -> 
            let (shape', typ', name') = match n.shape with
            | None ->
                let name, typ = add_name n.name in
                (None, typ, name)
            | Some sh ->
                let shape, typ = destruct ~ctx sh in
                (Some shape, typ, n.name)
            in
            Param.{
                typ = typ';
                value = Param.Named {name = name'; shape = shape'}
            }
        | Param.Extension _ -> (raise Common.TODO)


    let to_type params = 
        List.map params ~f: Param.(fun {typ; value; _} ->
            match value with
                | Positional _ -> Type.PosParam typ
                | Named {name; _} -> Type.NamedParam {param_typ = typ; is_optional = false; param_name = name.given.value}
                | Optional {name; _} -> Type.NamedParam {param_typ = typ; is_optional = true; param_name = name.given.value}
                | Extension _ -> (raise Common.TODO) (* flatmap struct here *)
    )

    let to_lambda params result = 
        Type.Lambda.make (to_type params) result
end *)

(* let resolve_typ typ = 
    let rec resolve = function
    (* TODO: Pay attention to vars up scope *)
    (* | Type.Invalid -> Type.Invalid *)
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


let foreign ~ctx f = 
    (* TODO: move to transform? *)
    let ti = type_ident ~ctx Foreign.(f.type_ident) in
    let typ = type_from_ident ti in
    let constr = Type.free_vars typ |> Set.to_list in
    Node.Foreign.{
        f with scheme = (Type.make_scheme constr typ)
    }

let ident ctx id = 
    let given = Qualified.given Ident.(id.qual) in
    match Scope.lookup Ctx.(ctx.scope) Ident.(id.qual) with
    | Some ({binding = Some binding; _}, resolved) -> 
        (* TODO: continue here: local bindings use their resolved, not exposed values *)
        let res, scheme = (match binding with
            | Resolved.Binding.Local loc -> (Id.local loc), None
            | Resolved.Binding.Global g -> g.id, Some g.scheme
        ) in
        let qual = Qualified.resolve_name res resolved in
        Node.Ident.{ id with
            scheme;
            qual
        }
        
    | Some ({binding = None; _}, _) -> 
    (* TODO: rly *)
        Ctx.add_errors ~ctx [Errors.UndeclaredIdentifier {given_name = given; range = id.range}];
        id
    | None -> 
        Ctx.add_errors ~ctx [Errors.UndeclaredIdentifier {given_name = given; range = id.range}];
        id *)

(* let rec local_binding root_ctx node = 
    let ctx' = Ctx.sub_ctx root_ctx (Resolver.Scope.sub_local) in
    let params = List.map ~f:(Params.resolve ~expr: (expr ctx') ~ctx: ctx') Let.(node.params) in
    (* TODO: remove *)
    let result = (root_ctx.tempvar()) in
    match Let.(node.is_rec) with
    | true ->
        let name = Scope.add_local_binding root_ctx.scope Let.(node.given_name) in
        { node with 
            params = params;
            scheme = None;
            block = block ctx' Let.(node.block); 
            scope_name = name;
            result = result
        }
    | false ->
        let b = block ctx' Let.(node.block) in
        let name = Scope.add_local_binding root_ctx.scope Let.(node.given_name) in
        { node with 
            scheme = None;
            block = b;
            scope_name = name;
            params = params;
            result = result
        } *)

(* let binding ctx node = 
    let params = List.map ~f:(Params.resolve ~expr: (expr ctx) ~ctx: ctx) Let.(node.params) in
    (* TODO: remove *)
    (if Let.(node.is_rec) then 
        ignore @@ Scope.add_local_binding root_ctx.scope Let.(node.given_name));
    block ctx' Let.(node.block)

        { node with 
            params = params;
            scheme = None;
            block = block ctx' Let.(node.block); 
            scope_name = "";
        }
    | false ->
        let b = block ctx' Let.(node.block) in
        { node with 
            block = b;
            params = params;
            result = result
        } *)

(* and matc ~ctx n =
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
            let id = Scope.add_local_binding ctx.scope p.given_name in
            Match.Param {p with scope_name = id; typ}
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
    | Stmt.Using u ->
        let using, errors = Resolve_using.using ctx.resolve_source ctx.scope u in
        (match errors with 
            | [] -> () 
            | errors -> Ctx.add_errors ~ctx errors;
        );
        Stmt.Using (using)
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
and apply ctx n = 
    let fn = expr ctx n.fn in
    let args = List.map ~f: (arg ctx) n.args in
    Apply.{n with fn; args; typ = ctx.tempvar()}
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
    Lambda.{ n with params = typed_params; block = b; typ = params_type} *)

