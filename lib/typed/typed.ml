open Base
open Common
include Node
(*
REFACTOR:
 - (substs, type) stuff occurs frequently. move it to struct
 - move all stuff into separate modules
 *)

module Subst = Subst
module Type = Type 
module Node = Node
module Type_util = Type_util
module Inferno = Inferno

let map_fst fn tuple = 
    let (fst, snd) = tuple in (fn fst, snd)

let stateful_map ~init ~f list  = 
    let (result, end_state) = List.fold list ~init: ([], init) ~f: (fun (result, state) item ->
        let (mapped, state') = f state item in
        (mapped :: result, state')
    ) in (List.rev result, end_state)
    

module Symbol = Symbol

module Resolver = Resolver
module Param = Param
module Base_types = Base_types
module Resolve = Resolve
module Transform = Transform
module Errors = Errors
module Util = Util
module Infer_match = Infer_match

module MyApply = struct 

    open Type 

    let rec lambda label = function
        | Lambda (from, to') ->
            let next () = lambda label to'
                |> Option.map ~f: (fun (result, next') -> (result, Lambda (from, next')))
            in
            (match (label, from) with
            | "", PosParam p -> Some (p, to')
            | "", NamedParam p ->
                Common.log["=="; p.name; label];
                next()
            | _, PosParam _  -> 
                next()
            | label, NamedParam p -> 
                if (String.equal p.name label) 
                then (Some (p.named_typ, to')) 
                else next()
            | _, NamedBlock _ -> (raise Common.TODO) )
        | _ -> None

end

(* TODO: move somewhere *)
module Infer = struct 
    open Inferno

    let unify_var ~ctx ~typ ~range expr ~f = 
        let unified = f expr in
        ignore @@ do_unify ~ctx ~error: (TypeMismatch {
            range = range;
            type_expected = typ;
            type_provided = unified;
        }) typ unified;
        unified

    let unify_result = unify_var

    (* TODO: get rid of ctx everywhere. it's a really bad idea. rly? it seems it's awesome *)
    let rec cond ~ctx n = 
        let infer_case ~ctx unify_with case' =
            let infered_if = block ~ctx Node.Cond.(case'.if_) in
            let errors = unify_ctx ~ctx Base_types.bool infered_if in
            if List.length errors > 0 then 
                ctx_add_errors ~ctx [Errors.IfTypeMismatch { range = Block.last_stmt_range case'.if_; unexpected = infered_if }];
                
            let infered_then = block ~ctx case'.then_ in
            match unify_with with
            | Some t ->
                let errors = unify_ctx ~ctx t infered_then in
                if List.length errors > 0 then begin
                    ctx_add_errors ~ctx [Errors.BranchTypeMismatch {
                        range = Block.last_stmt_range case'.if_;
                        expected = t;
                        unexpected = infered_then
                    }];
                    Some t
                end else
                    Some infered_then
            | None -> 
                Some infered_then
        in
        let unify_else unify_with else_ = 
            let infered_else = block ~ctx else_ in
            let errors = unify_ctx ~ctx unify_with infered_else in
            if List.length errors > 0 then begin 
                ctx_add_errors ~ctx [
                    Errors.BranchTypeMismatch {
                        range = Block.last_stmt_range else_;
                        expected = unify_with;
                        unexpected = infered_else;
                    }
                ]
            end;
            unify_with
        in
        let unify_with = match Cond.(n.else_) with
        | Some _ -> None
        | None -> Some Base_types.unit
        in
        let cases = List.fold Cond.(n.cases) ~init: unify_with ~f: (infer_case ~ctx) in
        match n.else_ with
        | Some else_ -> 
            unify_else (Option.value_exn cases) else_
        | None -> 
            Base_types.unit

    and block_stmts ~ctx stmts =
        let result_stmt ~ctx = function
            | Stmt.Expr e -> expr ~ctx e 
            | _ -> raise Common.TODO
        in

        let intern_stmt ~ctx = function
            | Stmt.Expr e -> ignore @@ expr ~ctx e
                (* TODO *)
                (* begin match Type.equals typ Base_types.unit with
                | true -> ()
                | false -> 
                    Common.log ["<><><>\n\n"; Type.to_string typ; Type.to_string Base_types.unit];
                    Basket.put ctx.errors @@ IgnoredResult {unexpected = typ}
                end; *)
            | Stmt.Let _let -> ignore @@ binding ~ctx _let
            | _ -> raise Common.TODO
        in

        let rec infer_stmt ~ctx = function
        | [] -> Base_types.unit
        | last :: [] -> 
            result_stmt ~ctx last
        | stmt :: rest -> 
            ignore @@ intern_stmt ~ctx stmt;
            infer_stmt ~ctx rest
        in
            infer_stmt ~ctx stmts

    and block ~ctx block = block_stmts ~ctx Block.(block.stmts)

    and infer_default_params ~ctx params = List.map params ~f: (fun Param.{typ; value} -> match value with
        | Param.Optional {default = e; _} -> Option.map e ~f: (fun e ->
            let default_typ = expr ~ctx e in
            (* TODO: check unification order *)
            do_unify ~ctx typ default_typ ~error: (
                Errors.TypeMismatch {
                    range = (Expr.range e);
                    type_expected = typ;
                    type_provided = default_typ
                };
            )
        )
        | _ -> None;
    ) 

    and lambda ~ctx lam =
        (* here all args are either untyped or have resolved values (or errors???)*)
        (* TODO: add *)
        (* TODO: result struct instead of this crappy tuple *)
        (* let env_with_args = Lambda.(lam.args) |> *)
        ignore @@ infer_default_params ~ctx Lambda.(lam.params);
        let block_result = block ~ctx Lambda.(lam.block) in
        let rec loop = function
        | Type.Lambda (_, to') -> loop to'
        | lam_result -> ignore @@ do_unify ~ctx block_result lam_result ~error: (
            (* TODO: what really to report? is it error of the last statement or the while block *)
            Errors.TypeMismatch {
                range = Lambda.(lam.range);
                type_expected = lam_result;
                type_provided = block_result
            });
        in loop lam.typ;
        (* should we do something with result? *)
        lam.typ

    and binding ~ctx n =
        (* TODO: unify them *)
        if Let.(n.is_rec) then 
            let result = ctx.tempvar() in
            (* two env' in same scope. wtf *)
            set_env ~ctx n.scope_name (Type.make_scheme [] result);
            ignore @@ infer_default_params ~ctx n.params;
            let b = block ~ctx Let.(n.block) in
            (* TODO: unify with let type *)
            (* TOOD: really, how about not @ Map.mem?  *)
            set_env ~ctx n.scope_name (Type.make_scheme (free_vars ~ctx b) result);
        else begin 
            ignore @@ infer_default_params ~ctx n.params;
            let typ = block ~ctx Let.(n.block) in
            (* TODO: unify with let type *)
            (* TOOD: really, how about not @ Map.mem?  *)
            set_env ~ctx n.scope_name 
                (Type.make_scheme (free_vars ~ctx typ) typ)
        end


    and tuple_expr ~ctx t = 
        match Tuple.(t.exprs) with
        | [] -> Base_types.unit
        | single :: [] -> expr ~ctx single
        | exprs ->
             Type.Tuple (List.map exprs ~f:(fun node ->
             (* why do we need to apply substs? *)
                let e = expr ~ctx node in apply~ctx e
            ))

    and new_apply ~ctx ~expr app =
        let step ~range var arg = 
            let label, e = (match arg with
                | Apply.PosArg {expr = e} ->
                    "", e
                | Apply.NameArg {expr = e; name} ->
                    name.value, e
            ) in
            let (from, to') = match var with
                | Type.Var v ->
                    let from_typ = ctx.tempvar() in
                    let result = ctx.tempvar() in
                    let src = (match label with
                        | "" -> Type.PosParam (from_typ)
                        | label -> Type.NamedParam {is_optional = false; name = label; named_typ = from_typ}
                    ) in
                    add_subst ~ctx v (Type.Lambda (src, result));
                    (from_typ, result)
                | Type.Lambda lam -> 
                    (match MyApply.lambda label (Type.Lambda lam) with
                        | Some r -> r
                        | None -> 
                            add_error ~ctx (match label with
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
                            (Type.Invalid, Type.Invalid)
                        )
                | t -> (
                    add_error ~ctx (
                        NotFunction {
                            range = range;
                            type_provided = t
                        }
                    );
                    Type.Invalid, Type.Invalid
                )
            in
            let expr_typ = expr e in
            ignore @@ do_unify ~ctx from expr_typ ~error: (
                TypeMismatch {
                    range = Expr.range e;
                    type_expected = from;
                    type_provided = expr_typ
                }
            );
            Subst.apply ctx.substs to'
        in 
        let fn_typ = expr Apply.(app.fn) in
        let app_typ, _ = List.fold Apply.(app.args) ~init: (fn_typ, (Expr.range app.fn)) ~f: (fun (typ, range) arg -> 
            let typ' = step ~range typ arg in
            let range' = Span.merge range (Node.Apply.arg_range arg) in
            (typ', range')
        ) in app_typ

    and apply_var ~ctx v m = 
        let result = ctx.tempvar() in
        let rec mk = function
            | [] -> result
            | arg :: rest -> Type.Lambda (arg, mk rest)
        in 
        let lam = mk @@ List.map Node.Apply.(m.args) ~f: (function 
            | PosArg {expr = e} -> Type.PosParam (expr ~ctx e)
            | NameArg {name; expr = e} -> Type.NamedParam {is_optional = false; name = name.value; named_typ = expr ~ctx e}
        ) in
        add_subst ~ctx v lam;
        result

    and apply_lambda ~ctx lam m =
        let rec do' = function
        | arg :: args, (Type.Lambda _ as lam) -> begin
            let result = (match arg with
            | Apply.PosArg {expr = e} ->
                (match MyApply.lambda "" lam with
                    | Some (typ, rest) -> Ok (e, typ, rest)
                    | None -> Error (Errors.CannotApplyWithoutLabel{
                        range = Expr.range e;
                        lambda = lam;
                    })
                )
            | Apply.NameArg {name; expr = e} ->
                (match (MyApply.lambda name.value lam) with
                    | Some (typ, rest) -> Ok (e, typ, rest)
                    | None -> Error (Errors.CannotApplyWithLabel{
                        range = Expr.range e;
                        label = name.value;
                        lambda = lam;
                    }))) in
            (match result with 
                | Ok (e, typ, rest) -> 
                    let node_typ = expr ~ctx e in
                    ignore @@ do_unify ~ctx (apply~ctx typ) node_typ ~error: (
                        TypeMismatch {
                            range = Expr.range e;
                            type_expected = typ;
                            type_provided = node_typ
                        }
                    );
                    (match args with
                    | [] -> Some rest
                    | next_args -> do'(next_args, rest))
                | Error e -> 
                    ctx_add_errors ~ctx [e];
                    Some (Type.Unknown) (* TODO: handle this case *)
            );
        end
        | args, t ->
            ctx_add_errors ~ctx [
                NotFunction {
                    range = Span.merge 
                        (Node.Apply.arg_range @@ List.hd_exn args)
                        (Node.Apply.arg_range @@ List.last_exn args);
                    type_provided = t
                }
            ];
            (* possible source of bugs. check the behaviour in production *)
            List.iter args ~f:(function
                | PosArg {expr = e} ->  ignore @@ expr ~ctx e
                | NameArg {expr = e; _} ->  ignore @@ expr ~ctx e
            );
            Some t
        in
        do' (Apply.(m.args), Type.Lambda lam) 
            |> Option.value ~default: (Type.Unknown)

    and apply_expr ~ctx m = unify_var ~ctx ~typ:Apply.(m.typ) ~range: (m.range) m ~f:(fun m ->
        new_apply ~ctx ~expr: (expr ~ctx) m 
    )

    and list ~ctx n = 
        match Li.(n.items) with
        | [] -> (Base_types.list (ctx.tempvar()))
        | single :: [] ->
            Base_types.list (expr ~ctx single)
        | first :: rest ->
            let first_t = expr ~ctx first in
            let _ = List.map rest ~f: (fun e ->
                let t = expr ~ctx e in
                (* TODO: make a generic function to skip errors *)
                let errors = unify_ctx ~ctx first_t t in
                (if List.length errors > 0 then (
                    ctx_add_errors ~ctx [
                        Errors.BranchTypeMismatch {
                            range = Expr.range e;
                            expected = first_t;
                            unexpected = t;
                        }
                    ]
                ));

                first_t
            ) in Base_types.list (first_t)

    and foreign ~ctx _ = ctx.tempvar()
    and expr ~ctx = function
        | Expr.Value m -> m.typ
        | Expr.Ident m -> Ident.(m.typ)
        | Expr.Lambda lam -> lambda ~ctx lam
        | Expr.Tuple t -> tuple_expr ~ctx t
        | Expr.Apply m -> apply_expr ~ctx m
        | Expr.Cond c -> cond ~ctx c
        | Expr.Foreign f -> foreign ~ctx f
        | Expr.Match m -> matc ~ctx m
        | Expr.Li l -> list ~ctx l
    and matc ~ctx m = Infer_match.matc ~ctx ~expr ~block_stmts m
end

(* TODO: better name *)
module Global = struct 
    let make_scheme typ =
        let _, mapping = List.fold_map (Type.free_vars typ |> Set.to_list) 
            ~init: ('a', 0)
            ~f: (fun (ch, n) var ->
                let v = (Char.to_string ch) ^ (if n = 0 then "" else Int.to_string n) in
                let next_ch = (Caml.Char.code ch) + 1 in
                let next = if next_ch > (Caml.Char.code 'z') then ('a', n + 1) else (Caml.Char.chr next_ch, n) in
                next, (var, v)
            )
        in 
        let subst = List.fold mapping ~init: (Subst.empty) ~f: (fun subst (replace, v) -> Subst.add subst replace (Type.Var v)) in
        Type.make_scheme (List.map mapping ~f: (fun (_, v) -> v)) (Subst.apply subst typ)

    let binding ~source resolver node = 
        let ctx = Resolve.Ctx.make ~source resolver in
        let resolved = Resolve.toplevel_binding ctx node in
        let scheme = (Option.value_exn resolved.scheme) in
        let env = match Let.(resolved.is_rec) with
        | true -> 
            Map.add_exn (Map.empty(module String)) 
                ~key: Let.(resolved.scope_name) 
                ~data: scheme
        | false -> Map.empty(module String)
        in
        let infer_ctx = Inferno.make_ctx ~env in
        (* TODO: return () *)
        ignore @@ Infer.infer_default_params ~ctx:infer_ctx resolved.params;
        let result_type = Infer.block ~ctx: infer_ctx Let.(resolved.block) in
        let result_type = Inferno.apply~ctx: infer_ctx result_type in
        let result_errors = Inferno.unify_ctx ~ctx: infer_ctx (resolved.result) result_type in 
        let result_params = Subst.apply_substs_to_params infer_ctx.substs resolved.params in
        let block = Subst.apply_to_block infer_ctx.substs resolved.block in
        let lambda_params = Resolve.Params.to_type result_params in
        let result = result_type in
        let lambda_typ = match lambda_params with
        | [] -> result
        | multiple -> Type.Lambda.make multiple result 
        in
        let scheme = make_scheme lambda_typ in
        Resolver.Scope.set_binding_scheme resolver Let.(resolved.given_name) scheme;
            (* TODO: proper names *)
        let typed_node = Let.{ resolved with 
            scheme = Some scheme; 
            params = result_params;
            result = Subst.apply infer_ctx.substs resolved.result;
            block 
        } in
        let resolve_errors = ctx.errors in
        let infer_errors = infer_ctx.errors in
        let errors = List.map (resolve_errors @ result_errors @ infer_errors) ~f:(Subst.apply_to_error infer_ctx.substs) in
        (typed_node, errors)
    
    let expose modu_id entries = 
        let make_id scope_name = Symbol.Id.make Symbol.Id.(modu_id.source) (modu_id.modules @ [modu_id.name]) scope_name in
        entries
        |> List.filter ~f: (function 
            | Module.Module m -> not @@ String.is_prefix ~prefix:"_" m.given_name
            | Module.Binding b -> not @@ String.is_prefix ~prefix:"_" b.given_name
            | Module.Import _ -> false
        )
        |> List.fold ~init: (Symbol.Module.empty modu_id) ~f: (fun sym node -> 
            let given_name = (match node with 
                    | Module.Module m -> m.given_name
                    | Module.Binding b -> b.given_name
                    | _ -> raise Common.Unreachable) in 
            { sym with exposed = Map.update sym.exposed given_name
                ~f: (fun exposed -> 
                    let exposed = (Option.value exposed ~default: (Symbol.Module.empty_exposed)) in
                    (match node with 
                    | Module.Module m -> 
                        { exposed with modu = Some (Symbol.Module.make (make_id m.scope_name) m.exposed) } 
                    | Module.Binding b -> 
                        { exposed with binding = Some (Symbol.Binding.make (make_id b.scope_name) (Option.value ~default: Type.unknown_scheme b.scheme)) } 
                    | _ -> raise Common.Unreachable))
            } 
        ) 

    let rec modu ~resolve_source resolver id m = 
        (* TODO: Symbol.Module / Typedef.Module / Def.Module *)
        (* TODO: Symbol.Id / some global id *)
        (* s/typedefs/typs in Symbol.t: typedef is type definition *)
        match try_map Node.Module.(m.entries) ~f: (function
            | Node.Module.Module m -> 
                let id = Resolver.Scope.make_id resolver m.given_name in
                let m = {m with scope_name = id.name} in
                let res = Resolver.Scope.sub_module id.name resolver in
                (match modu ~resolve_source res id m with
                | (Error e) -> Error e
                | (Ok tm) -> 
                    let modu = Symbol.Module.{
                        id; exposed = Module.(tm.exposed)
                    } in
                    Resolver.Scope.set_resolution resolver m.given_name (fun res -> { res with
                        modu = Some modu;
                    });
                    Ok (Module.Module tm))
            | Node.Module.Binding b ->
                let (typ, errs) = binding ~source: id.source resolver b in
                (match errs with 
                    | [] -> Ok (Module.Binding typ)
                    | _ -> Error errs)
            | Node.Module.Import im -> 
                let (import, errs) = Resolve.import resolve_source resolver im in
                (match errs with
                | [] -> Ok (Module.Import import)
                | errs -> Error errs)
        ) with
            | Error errors -> Error errors
            (* TODO: return map not module *)
            | Ok entries -> let exposed = expose id entries in
                Ok (Module.{ m with entries; exposed = exposed.exposed })
end

let root ~source ~resolve_source m = 
    let int_op = Type.lambda [Base_types.int; Base_types.int; Base_types.int] in
    let cmp_op = Type.lambda [Base_types.int; Base_types.int; Base_types.bool] in
    let predefined = [
        "foreign", Type.lambda ~constr: ["t"; "r"] [Base_types.str; Type.Var "t"; Type.Var "r"];
        "println", Type.lambda ~constr: ["t"] [Type.Var "t"; Base_types.unit];
        "+", int_op;
        "-", int_op;
        "*", int_op;
        "/", int_op;
        ">", cmp_op;
        "<", cmp_op;
        "==", cmp_op;
        "!=", cmp_op;
        "%", int_op;
        "$", Type.lambda ~constr: ["t"; "u"] [Type.Lambda.make_positional [Type.Var "t"; Type.Var "u"]; Type.Var "t"; Type.Var "u"];
        "|>", Type.lambda ~constr: ["t"; "u"] [Type.Var "t"; Type.Lambda.make_positional [Type.Var "t"; Type.Var "u"]; Type.Var "u"];
    ] in
    let resolver = Resolver.Scope.root source in
    List.iter predefined ~f:(fun (name, scheme) -> 
        ignore @@ Resolver.Scope.implant_binding resolver name scheme);
    (* TODO: pass name *)
    let tf = Transform.root m in 
    let m = Global.modu ~resolve_source resolver (Symbol.Id.make source [] "") Node.Module.{scope_name = ""; given_name = ""; exposed = Map.empty(module String); entries = tf.entries} in
    (match m with
    | Ok m -> Common.log["~~~"; Pp.to_string [m |> Node.Print_node.modu]];
    | Error _ -> ());
    m