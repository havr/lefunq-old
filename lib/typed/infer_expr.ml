open Base
open Common
open Node
open Typed_common

let instantiate_scheme ~ctx scheme =
    let new_substs = List.fold 
        Type.(scheme.constr) 
        ~init: Subst.empty
        ~f: (fun substs var_name ->
            Subst.add substs var_name Infer_ctx.(ctx.tempvar())
    ) in Subst.apply new_substs scheme.typ

let foreign ~ctx f = 
    let ti = Typed_type.type_ident ~ctx Foreign.(f.type_ident) in
    let typ = Typed_type.type_from_ident ti in
    let constr = Type.free_vars typ |> Set.to_list in
    let scheme = Type.make_scheme constr typ in
    let typ = instantiate_scheme ~ctx scheme in
    let node = Node.Foreign.{
        f with scheme; typ
    } in node, typ

let lambda_type params ret = 
    match Infer_params.to_type params with
        | [] -> ret
        | multiple -> Type.Lambda.make multiple ret

let generalize_lambda_scheme parent_env params ret =
    let typ = lambda_type params ret in
    let free = Set.diff (Type.free_vars typ) (Infer_env.free_vars parent_env)
        |> Set.to_list in
    Type.make_scheme free typ

let free_var_rename_substs free = 
    let vars = List.init (List.length free) ~f: (fun n ->
        let chrn, num = (n % 26, n / 26) in
        let char = Char.to_string @@ Char.of_int_exn (Char.to_int 'a' + chrn) in
        let num' = if num = 0 then "" else Int.to_string num in
        char ^ num'
    ) in
    let substs = List.zip_exn free vars
        |> List.fold ~init: (Subst.empty) ~f: (fun subst (old, var) -> 
            Subst.add subst old (Type.Var var))
    in vars, substs

let ident ~ctx node =
    let resolved =
        let given = Qualified.given Ident.(node.qual) in
        match Scope.lookup Infer_ctx.(ctx.scope) node.qual with
        | Some ({binding = Some (Resolved.Binding.Local loc); _}, resolved) -> 
            Ok ((Qualified.resolve_name (Id.local loc) resolved), (Infer_ctx.lookup_env ~ctx loc))
        | Some ({binding = Some (Resolved.Binding.Global g); _}, resolved) -> 
            Ok ((Qualified.resolve_name g.id resolved), g.scheme)
        | Some ({binding = None; _}, _) -> 
            Error (Errors.UndeclaredIdentifier {given_name = given; range = node.range})
        | None -> 
            Error (Errors.UndeclaredIdentifier {given_name = given; range = node.range})
    in
    match resolved with
    | Ok (qual, scheme) ->
        let typ = instantiate_scheme ~ctx scheme in
        let node = {node with qual; typ; scheme = Some scheme} in
        node, typ
    | Error e -> 
        Infer_ctx.add_error ~ctx e;
        (* TODO: spawn an weak invalid typ *)
        (node, Type.Unknown)

(* TODO: get rid of ctx everywhere. it's a really bad idea. rly? it seems it's awesome *)

(* TODO: put sub ctxs everwhere and check it! *)
let rec cond ~ctx n = 
    let infer_case ~ctx unify_with case' =
        let if_node, infered_if = block ~ctx Node.Cond.(case'.if_) in
        Infer_ctx.unify ~ctx Base_types.bool infered_if ~error: (
            Errors.IfTypeMismatch { range = Block.last_stmt_range case'.if_; unexpected = infered_if }
        );
        let then_node, infered_then = block ~ctx case'.then_ in
        let typ = match unify_with with
        | Some t ->
            let ok = Infer_ctx.try_unify ~ctx t infered_then ~error: (
                Errors.BranchTypeMismatch {
                    range = Block.last_stmt_range case'.if_;
                    expected = t;
                    unexpected = infered_then
                }
            ) in
            if ok then (Some infered_then) else (Some t)
        | None -> 
            Some infered_then
        in typ, Node.Cond.{if_ = if_node; then_ = then_node}
    in
    let unify_else unify_with else_ = 
        let else_node, infered_else = block ~ctx else_ in
        Infer_ctx.unify ~ctx unify_with infered_else ~error: (
            Errors.BranchTypeMismatch {
                range = Block.last_stmt_range else_;
                expected = unify_with;
                unexpected = infered_else;
            }
        );
        else_node, unify_with
    in
    let unify_with = match Cond.(n.else_) with
    | Some _ -> None
    | None -> Some Base_types.unit
    in
    let cases_typ, cases = List.fold_map Cond.(n.cases) ~init: unify_with ~f: (infer_case ~ctx) in
    match n.else_ with
    | Some else_ -> 
        let else_node, typ = unify_else (Option.value_exn cases_typ) else_ in
        Cond.{cases = cases; else_ = Some else_node; typ; range = n.range}, typ
    | None -> 
        let typ = Base_types.unit in
        Cond.{cases = cases; else_ = None; typ; range = n.range}, typ

and block_stmts ~ctx stmts =
    let result_stmt ~ctx = function
        | Stmt.Expr e -> 
            let node, typ = expr ~ctx e in
            (Stmt.Expr node), typ
        | _ -> raise Common.TODO
    in

    let intern_stmt ~ctx = function
        | Stmt.Expr e -> 
            let typed, _ = expr ~ctx e in
            Stmt.Expr typed
        | Stmt.Let _let -> 
            Stmt.Let (binding ~ctx _let)
        | Stmt.Block _ -> raise Common.TODO
        | Stmt.Using u -> (
            let using, errors = Infer_using.using Infer_ctx.(ctx.resolve_source) ctx.scope u in
            (match errors with 
                | [] -> () 
                | errors -> List.iter errors ~f:(Infer_ctx.add_error ~ctx)
            );
            Stmt.Using (using)
        )
    in

    let rec infer_stmt ~ctx result = function
    | [] -> [], Base_types.unit
    | last :: [] -> 
        let n, typ = result_stmt ~ctx last in
        (result @ [n]), typ
    | stmt :: rest -> 
        let n = intern_stmt ~ctx stmt in
        infer_stmt ~ctx (result @ [n]) rest
    in
        infer_stmt ~ctx [] stmts

and block ~ctx block = 
    let stmts, typ = block_stmts ~ctx Block.(block.stmts) in
    Block.{stmts; range = block.range}, typ

and lambda ~ctx:pctx lam =
    let ctx = Infer_ctx.sub_local pctx in
    let params = infer_params ~ctx Lambda.(lam.params) in
    let block, ret = block ~ctx lam.block in
    let typ =  (Infer_ctx.apply ~ctx (lambda_type params ret)) in
    Lambda.{ lam with typ; params; block}, typ

and infer_params ~ctx params = 
    List.map params ~f:(Infer_params.resolve ~ctx ~expr: (fun e ->
        let n = expr ~ctx:(Infer_ctx.sub_local ctx) e in n
    ))


and let_lambda ~ctx parent_env params bl =
    (* let parent_env = !(ctx.env) in *)
    let block, ret = block ~ctx bl in
    let typ =  (Infer_ctx.apply ~ctx (lambda_type params ret)) in
    let free = Set.diff (Type.free_vars typ) (Infer_env.free_vars (Infer_env.apply_substs !(ctx.substs) parent_env))
        |> Set.to_list in
    Common.log ["@@"; String.concat ~sep:";" free];
    let vars, fvsubsts = free_var_rename_substs free in
    let substs = Subst.combine (Subst.apply_to_substs !(ctx.substs) fvsubsts) fvsubsts in
    Common.log["##"; Type.to_string typ; Subst.to_string substs];
    let block = Subst.apply_to_block substs block in
    let params = Subst.apply_substs_to_params substs params in
    let typ = Subst.apply substs typ in
    let scheme = Type.make_scheme vars typ in
    scheme, params, block

and binding ~ctx:pctx n =
    let parent_env = !(pctx.env) in
    let ctx = Infer_ctx.sub_local pctx in
    (* TODO: make tests to replicate http error *)
    let params = infer_params ~ctx Let.(n.params) in
    let rec_scope_name = if n.is_rec then (
        let rec_scheme = generalize_lambda_scheme !(ctx.env) params (ctx.tempvar()) in
        let scope_name = Scope.add_local_binding pctx.scope n.given_name in
        Infer_ctx.add_env ~ctx:pctx scope_name rec_scheme;
        Some scope_name
    ) else (
        None
    ) in

    let scheme, params, block = let_lambda ~ctx parent_env params n.block in
    let scope_name = (match rec_scope_name with
    | Some sn -> sn
    | None ->
        let final_name = Scope.add_local_binding pctx.scope Let.(n.given_name) in
        Infer_ctx.add_env ~ctx final_name scheme;
        final_name
    ) in

    {n with scope_name; params; block; scheme = Some scheme}

and tuple_expr ~ctx n = 
    match Tuple.(n.exprs) with
    | single :: [] -> expr ~ctx single
    | exprs -> 
        let nodes, typs = 
            List.map exprs ~f: (expr ~ctx)
            |> List.unzip
        in Expr.Tuple (Tuple.{n with exprs = nodes}), Type.Tuple typs

and new_apply ~ctx ~expr app =
    let rec apply_lambda_typ label typ = 
        let open Type in 
        match typ with
        | Lambda (from, to') ->
            let next () = apply_lambda_typ label to'
                |> Option.map ~f: (fun (result, next') -> (result, Lambda (from, next')))
            in
            (match (label, from) with
            | "", PosParam p -> Some (p, to')
            | "", NamedParam _ -> next()
            | _, PosParam _  -> next()
            | label, NamedParam p -> 
                if (String.equal p.param_name label) 
                then (Some (p.param_typ, to')) 
                else next()
            | _, NamedBlock _ -> (raise Common.TODO))
        | _ -> None
    in
    let step ~range var arg = 
        let arg', expr_typ = (match arg with
            | Apply.PosArg p -> 
                let node, expr_typ = expr p.expr in
                (Apply.PosArg {expr = node}), expr_typ
            | Apply.NameArg p ->
                let node, expr_typ = expr p.expr in
                (Apply.NameArg {p with expr = node}), expr_typ
        ) in
        let label, e = (match arg with 
            | Apply.PosArg p -> 
                "", p.expr
            | Apply.NameArg p ->
                p.name.value, p.expr
        ) in
        let (from, to') = match var with
            | Type.Var v ->
                let from_typ = Infer_ctx.(ctx.tempvar()) in
                let result = ctx.tempvar() in
                let src = (match label with
                    | "" -> Type.PosParam (from_typ)
                    | label -> Type.NamedParam {is_optional = false; param_name = label; param_typ = from_typ}
                ) in
                Infer_ctx.unify_var ~ctx v (Type.Lambda (src, result));
                (* add_subst ~ctx v (Type.Lambda (src, result)); *)
                (from_typ, result)
            | Type.Lambda lam -> 
                (match apply_lambda_typ label (Type.Lambda lam) with
                    | Some r -> r
                    | None -> 
                        Infer_ctx.add_error ~ctx (match label with
                        | "" -> Errors.CannotApplyWithoutLabel {
                            range = Expr.range e;
                            lambda = Type.Lambda lam;
                        }
                        | label -> Errors.CannotApplyWithLabel {
                                range = Expr.range e;
                                label = label;
                                lambda = Type.Lambda lam;
                            }
                        );
                        (ctx.weakvar(), ctx.weakvar())
                    )
            | Type.Unknown ->
                (ctx.weakvar(), ctx.weakvar())
            | t -> (
                Infer_ctx.add_error ~ctx (
                    NotFunction {
                        range = range;
                        type_provided = t
                    }
                );
                (ctx.weakvar(), ctx.weakvar())
            )
        in
        Infer_ctx.unify ~ctx from expr_typ ~error: (
            TypeMismatch {
                range = Expr.range e;
                type_expected = from;
                type_provided = expr_typ
            }
        );
        arg', Subst.apply !(ctx.substs) to'
    in 
    let fn_node, fn_typ = expr Apply.(app.fn) in
    let (app_typ, _), arg_nodes = List.fold_map Apply.(app.args) ~init: (fn_typ, (Expr.range app.fn)) ~f: (fun (typ, range) arg -> 
        let node, typ' = step ~range typ arg in
        let range' = Span.merge range (Node.Apply.arg_range arg) in
        (typ', range'), node
    ) in {app with fn = fn_node; args = arg_nodes; typ = app_typ}, app_typ

and list ~ctx n = 
    let item ~ctx = function
        | Li.Single e ->
            let n, t = expr ~ctx e in
            n, Li.Single n, t, Base_types.list t
        | Li.Spread e ->
            let n, t = expr ~ctx e in
            let it = ctx.tempvar () in
            let list_t = Base_types.list it in
            Infer_ctx.unify ~ctx list_t t ~error: (
                Errors.ListTypeExpected {
                    range = Expr.range e;
                    got_unexpected = t;
                }
            );
            n, Li.Spread n, it, list_t
    in
    match Li.(n.items) with
    | [] -> Li.{n with items = []}, (Base_types.list Infer_ctx.(ctx.tempvar()))
    | single :: [] ->
        let _, node, _, li_typ = item ~ctx single in
        Li.{n with items = [node]}, li_typ
    | first :: rest ->
        let _, first_n, it_typ, _ = item ~ctx first in
        let rest_n = List.map rest ~f: (fun it ->
            let e, n, t, _ = item ~ctx it in
            (* TODO: make a generic function to skip errors *)
            Infer_ctx.unify ~ctx it_typ t ~error: (
                Errors.ListItemTypeMismatch {
                    range = Expr.range e;
                    expected = it_typ;
                    unexpected = t;
                }
            );
            n
        ) 
        in Li.{n with items = first_n :: rest_n}, Base_types.list (it_typ)

and expr ~ctx e = 
    match e with
    | Expr.Value m -> 
        (Expr.Value m), m.typ
    | Expr.Ident m -> 
        let typed, typ = ident ~ctx m in 
        (Expr.Ident typed), typ
    | Expr.Lambda lam -> 
        let typed, typ = lambda ~ctx lam in
        (Expr.Lambda typed, typ)
    | Expr.Tuple t -> 
        let expr, typ = tuple_expr ~ctx t in
        expr, typ
    | Expr.Li l ->
        let typed, typ = list ~ctx l in
        (Expr.Li typed, typ)
    | Expr.Apply m -> 
        let typed, typ = new_apply ~ctx ~expr: (expr ~ctx) m in
        (Expr.Apply typed, typ)
    | Expr.Cond c -> 
        let typed, typ = cond ~ctx c in
        (Expr.Cond typed, typ)
    | Expr.Foreign f -> 
        let node, typ = foreign ~ctx f in
        (Expr.Foreign node), typ
    | Expr.Match m -> 
        let typed, typ = matc ~ctx m in
        (Expr.Match typed, typ)

and matc ~ctx m = Infer_match.matc ~ctx ~expr ~block_stmts m
