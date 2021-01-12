open Base
open Common

include Node
(* type type_ = string *)
(* TODO: use typ instead of type' and type_ *)

module Type = Type 
module Node = Node

module BaseTypes = struct 
    let int_name = "Std.Base.Int"
    let int = Type.Simple int_name
    let str = Type.Simple "Std.Base.Str"
    let unit = Type.Simple "Std.Base.Unit"
    let bool_name = "Std.Base.Bool"
    let bool = Type.Simple bool_name
end

type tempvar_gen = unit -> Type.t

let make_tempvar_gen prefix = 
    let idx = ref 0 in fun () ->
        let result = Type.Var (prefix ^ (Int.to_string !idx)) in
        idx := !idx + 1;
        result

let map_fst fn tuple = 
    let (fst, snd) = tuple in (fn fst, snd)

let stateful_map ~init ~f list  = 
    let (result, end_state) = List.fold list ~init: ([], init) ~f: (fun (result, state) item ->
        let (mapped, state') = f state item in
        (mapped :: result, state')
    ) in (List.rev result, end_state)
    

module Basket = struct 
    type 'u t = {
        mutable items: 'u list
    }

    let make () = {items = []}

    let put basket item = 
        basket.items <- item :: basket.items

    let get basket = basket.items
end

module Resolver = Resolver
module Param = Param

module Resolve = struct
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
            ) in

        (* let arg_shape = Param.(arg.shape) in
        let arg_typ = Param.(arg.typ) in *)
        match (shape, typ) with
        | Param.Unit, Type.Unknown ->
            Param.{shape = Param.Unit; type' = BaseTypes.unit }
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
        | Param.Tuple _, _ -> raise Common.TODO
            (* maybe other_type is actually a tuple *)
            (* but now just throw an error *)

    let type_params tempvar params = 
        List.map params ~f:(fun param -> 
            type_param tempvar Param.(param.type') Param.(param.shape)
        )

    type error = { given_name: string; pos: Common.Pos.t }

    open Resolver

    type context = {
        (* LocalResolver? *)
        resolver: Local.t;
        errors: error Basket.t;
        tempvar: tempvar_gen;
    }

    let make_context resolver = {
        resolver = resolver;
        errors = Basket.make();
        tempvar = make_tempvar_gen "p";
    }

    let with_sub_resolver ctx name = {
        ctx with resolver = Local.sub ~name: name ctx.resolver
    }

    let ident ctx id = 
        let resolution = Local.lookup ctx.resolver Node.Ident.(id.given_name) in
        match resolution with
        | Some (`Global scheme) -> 
            match name with

            (* TODO: global ids *)
            { id with 
                Node.Ident.resolved = Node.Ident.Local {
                    scope_name = name;
                    param = false;
                }
            }
        | None -> 
            Basket.put ctx.errors {given_name = id.given_name; pos = id.pos};
            id
    
    let rec binding ctx node = 
        let (scope_name, with_name) = Local.add_name ctx.resolver Let.(node.given_name) (Type.make_scheme [] Type.Unknown) in
        let use_resolver = if node.is_rec then {ctx with resolver = with_name} else ctx in
        ({node with 
            block = block use_resolver Let.(node.block); 
            scope_name = scope_name
        }, {
            ctx with resolver = with_name
        })

    and block ctx n =
        let (stmts, _) = stateful_map Block.(n.stmts) 
            ~init: ctx 
            ~f:(fun ctx -> function 
                | Stmt.Expr n -> 
                    (Stmt.Expr (expr ctx n), ctx)
                | Stmt.Let n -> 
                    let (result, new_ctx) = (binding ctx n) in
                    (Stmt.Let result, new_ctx)
                | Stmt.Block 
                    _ -> raise Common.TODO (* Disallow block inside blocks *)
        ) in Block.{stmts = stmts}
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
        Apply.{
          fn = fn_result;
          args = arg_results 
        }
    and cond ctx n = 
        let cases = List.map n.cases ~f:(fun {if_; then_} ->
            Cond.{
                if_ = block (with_sub_resolver ctx "TODO") if_;
                then_ = block (with_sub_resolver ctx "TODO") then_;
            }
        ) in
        let else_ = Cond.(n.else_) |> Option.map ~f: (block (with_sub_resolver ctx "else")) in
        Cond.{ cases = cases; else_ = else_ }
    and lambda ctx n = 
        (* TODO: resolve type names *)
        let rec param_name_list param = match Param.(param.shape) with
        | Param.Name n -> [n, Type.make_scheme [] param.type']
        | Param.Tuple t -> List.map t ~f:param_name_list |> List.concat
        | Param.Unit -> []
        in 
        
        let without_duplicates param_names =
            let as_map = List.fold param_names ~init: (Map.empty(module String)) ~f:(fun map (name, typ) ->
                if Map.mem map name then (* throw an error *) map
                else (Map.add_exn map ~key: name ~data: typ)) in
            Map.to_alist as_map
        in 
        
        let typed_params = type_params ctx.tempvar Lambda.(n.params) in
        let param_names = List.map typed_params ~f:param_name_list |> List.concat in
        let type_names = without_duplicates param_names in
        let resolver_with_params = List.fold type_names
            ~init: (Local.sub ctx.resolver ~name: "TODO")
            ~f: (fun resolver (name, scheme) -> 
                let (_, resolver') = Local.add_name resolver name scheme in resolver') 
        in Lambda.{
            params = Lambda.(n.params);
            block = block {ctx with resolver = resolver_with_params} n.block
        }

    (* let modu ~ctx m =
        let ctx = ref ctx in
        let entries' = List.map Node.Module.(m.entries) ~f: (function
            | Node.Module.Binding b -> 
                let (b', ctx') = binding (!ctx) b in
                ctx := ctx';
                Node.Module.Binding b'
        ) in
        {m with entries = entries'} *)
end

module Transform = struct
    (* TODO: remove these transform_ prefixes *)
    (* TODO: move Int / Str to constants *)
    (* TODO: resolve these Ast.Node.Stuff (use Node and TyNode?) *)
    let params args = 
        let rec map_arg = function
        | Ast.Node.Arg.Ident m -> 
            Param.{shape = Param.Name m.ident; type' = Type.Unknown}
        | Ast.Node.Arg.Tuple m -> begin 
            match m.tuple with 
            | [] -> Param.{shape = Param.Unit; type' = BaseTypes.unit}
            | name :: [] -> map_arg name
            | tuple ->
                Param.{shape = Param.Tuple (List.map tuple ~f:map_arg); type' = Type.Unknown}
            end 
        in List.map args ~f:map_arg

    let rec apply apply = 
        let fn = expr Ast.Node.Apply.(apply.fn) in
        let args = List.map ~f:expr Ast.Node.Apply.(apply.args) in
            Apply.{fn=fn; args=args}
    and value = function
        | Ast.Node.Value.Int v -> Expr.Value (Value.{value = v.value; type_ = BaseTypes.int})
        | Ast.Node.Value.Str v -> Expr.Value (Value.{value = v.value; type_ = BaseTypes.str})
        (* TODO: return AST without parlex pos *)
        | Ast.Node.Value.Ident v -> Expr.Ident (Ident.{
            given_name = v.value;
            resolved=Ident.Undefined;
            scheme = None;
            pos = v.pos |> Ast.from_parlex_pos
        })
        | Ast.Node.Value.Lambda v -> 
            let b = block v.block in
            let a = params v.args.args in
                Expr.Lambda (Lambda.{params = a; block = b})
        
    and cond n = 
        let expr_or_block = function
            (* TODO: variable names *)
            | Ast.Node.Cond.Expr e -> Block.{stmts = [Stmt.Expr (expr e)]}
            | Ast.Node.Cond.Block b -> block b
        in
        let case = Cond.{if_ = expr_or_block Ast.Node.Cond.(n.if_); then_ = expr_or_block Ast.Node.Cond.(n.then_)} in
        match n.else_ with
        | None ->
            Cond.{cases = [case]; else_ = None}
        | Some (Ast.Node.Cond.Expr (Ast.Node.Expr.Cond e)) ->
                let result = cond e in 
                {result with cases = case :: result.cases}
        | Some m -> Cond.{cases = [case]; else_ = Some (expr_or_block m)}
    and expr: Ast.Node.Expr.t -> Expr.t = function
        | Ast.Node.Expr.Value v -> value v
        | Ast.Node.Expr.Apply app -> Expr.Apply (apply app)
        | Ast.Node.Expr.Cond n -> Expr.Cond (cond n)
    and block_stmt = function
        | Ast.Node.Block.Expr e -> Stmt.Expr (expr e)
        | Ast.Node.Block.Block b -> Stmt.Block (block b)
        | Ast.Node.Block.Let t -> Stmt.Let (binding t)
    and block b = Block.{stmts = (List.map ~f:block_stmt b.stmts)}
    and binding_expr = function
        | Ast.Node.Let.Expr v -> Block.{stmts = [Stmt.Expr (expr v)]}
        | Ast.Node.Let.Block e -> block e
    and binding n = 
        (* todo: s/args/params in paraser*)
        let t_expr = match n.args with
            | None -> binding_expr n.expr
            | Some {args} ->
                let params = params args in
                let block = binding_expr (n.expr) in
                let stmt = Stmt.Expr(Expr.Lambda(Lambda.{params; block})) in
                Block.{stmts = [stmt]}
        in Let.{
            given_name = n.ident.value;
            scope_name = "";
            block = t_expr;
            is_rec = n.is_rec;
            scheme = None
        }
    
    let modu n =
        let entries = List.map n ~f: (fun n ->
            Node.Module.Binding (binding n)
        ) in
        Node.Module.{name = ""; entries = entries}
end

(* TODO: move somewhere *)
let root root_stmts = 
    (* TODO: sigs? *)
    let root = Resolver.Global.root () in
    let resolver = ["println"; "+"; "-"; "*"; "/"; ">"; "<"; "$"; "|>"; "=="; "!="; "%"] 
        |> List.fold ~init:(Resolver.Local.make root) ~f:(fun resolver name -> 
            let (_, resolver') = Resolver.Local.add_name resolver name (Type.make_scheme [] Type.Unknown) in 
            resolver'
        ) in
    let root_ctx = Resolve.make_context resolver in
    root_stmts |> stateful_map ~init:root_ctx  ~f:(fun ctx stmt ->
    (* let's make it work with let_ for now *)
        let typed_let = Transform.binding stmt in
        let (resolved, ctx') = Resolve.binding ctx typed_let in
        (resolved, ctx')
    ) 


module TypeStore = struct 
    type t = {
        mutable map: Type.t StringMap.t;
        mutable bind: string StringMap.t;
    }

    let make () = {
        map = Map.empty (module String);
        bind = Map.empty (module String);
    }
    let add st name def = 
        ignore (Map.set st.map ~key: name ~data: def)

    let bind st name typename =
        ignore (Map.set st.bind ~key: name ~data: typename)

    let get st name =
        Map.find_exn st.map name
end


let rec unroll_lambda (arg, ret) =
    match ret with
    | Type.Lambda ret -> arg :: (unroll_lambda ret)
    | t -> [t]

module Infer = struct 
    type env = Type.scheme StringMap.t

    type error = TypeMismatch of {
        type_expected: Type.t;
        type_provided: Type.t;
    } | NotFunction of {
        type_provided: Type.t
    } | IgnoredResult of {
        type_provided: Type.t
    } | IfTypeMismatch of {
        unexpected: Type.t
    } | BranchTypeMismatch of {
        unexpected: Type.t;
        expected: Type.t
    }

    let error_equals a b = match (a, b) with
    | TypeMismatch {type_expected = te; type_provided = tp},
      TypeMismatch {type_expected = te'; type_provided = tp'} ->
        (Type.equals te te' && Type.equals tp tp')
    | IgnoredResult {type_provided=tp}, IgnoredResult {type_provided=tp'} -> 
        Type.equals tp tp'
    | NotFunction {type_provided=tp}, NotFunction {type_provided=tp'} -> 
        Type.equals tp tp'
    | IfTypeMismatch {unexpected=u}, IfTypeMismatch {unexpected=u'} -> 
        Type.equals u u'
    | BranchTypeMismatch {unexpected=u; expected=e}, BranchTypeMismatch {unexpected=u'; expected=e'} -> 
        (Type.equals u u' && Type.equals e e')
    | _ -> false

    type context = {
        errors: error Basket.t;
        tempvar: tempvar_gen;
        store: TypeStore.t;
    }

    (* TODO: make_context *)
    let make_ctx () = {
        errors = Basket.make ();
        tempvar = make_tempvar_gen "t";
        store = TypeStore.make ()
    }

    let empty_subst = Map.empty (module String)

    let new_subst var typ = 
        Map.add_exn empty_subst ~key: var ~data: typ 

    let add_subst substs var typ = 
        Map.add_exn substs ~key: var ~data: typ 

    let substs_to_string substs = Map.to_alist substs |> List.map ~f:(fun (k, v) ->
        (k ^ "=" ^ Type.to_string v)
    ) |> String.concat ~sep:", "

    let combine_substs_f src dst =
        Map.fold dst ~init: (src, []) ~f: (fun ~key ~data (result, errors) ->
            match Map.find result key with
            | Some existing -> 
                if (Type.equals existing data) then
                    (result, errors)
                else 
                    (result, TypeMismatch {
                        type_expected=existing;
                        type_provided=data
                    } :: errors)
            | None -> 
                (Map.add_exn result ~key: key ~data: data, errors)
        )

    (* more elegant solution. carry them? *)
    let flush_errors ~ctx errors = List.iter errors ~f:(fun e -> Basket.put ctx.errors e)


    let combine_substs ~ctx src dst =
        let (substs, errors) = combine_substs_f src dst in
        flush_errors ~ctx errors;
        substs

    (* TODO: refactor *)
    type unify_result = {
        unified: Type.t StringMap.t;
        unify_errors: error list;
    }

    let rec unify ~ctx ta tb = 
        let mismatch = {
            unified = empty_subst;
            unify_errors = [
                TypeMismatch { type_expected = ta; type_provided = tb }
            ]
        } in match (ta, tb) with
        | (Type.Var va, _) -> 
            {unified = new_subst va tb; unify_errors = []}
        | (_, Type.Var vb) -> 
            {unified = new_subst vb ta; unify_errors = []} 
        | (Type.Simple sa, Type.Simple sb) ->
            if not @@ String.equal sa sb then mismatch else 
                {unified = empty_subst; unify_errors = []}
        | (Type.Tuple tua, Type.Tuple tub) ->
            if List.length tua <> List.length tub then mismatch else
            List.fold (List.zip_exn tua tub) 
                ~init: {unified=empty_subst; unify_errors=[]}
                ~f: (fun result (ia, ib) -> 
                    let {unified; unify_errors} = unify ~ctx ia ib in 
                    let (combined, combine_errors) = combine_substs_f result.unified unified in { unified = combined;
                        unify_errors = unify_errors @ result.unify_errors @ combine_errors;
                    })
        | (Type.Lambda la, Type.Lambda lb) -> begin
            let lau = unroll_lambda la in
            let lbu = unroll_lambda lb in
            if List.length lau <> List.length lbu then mismatch else
            List.fold (List.zip_exn lau lbu) 
                ~init: {unified=empty_subst; unify_errors=[]}
                ~f: (fun result (ia, ib) -> 
                    let {unified; unify_errors} = unify ~ctx ia ib in 
                    let (combined, combine_errors) = combine_substs_f result.unified unified in {
                        unified = combined;
                        unify_errors = unify_errors @ result.unify_errors @ combine_errors;
                    })
        end
        | _ -> 
            mismatch

    let rec apply_substs substs = function
    | Type.Var v -> begin match Map.find substs v with 
        | Some t -> t
        | None -> Type.Var v
    end
    | Type.Lambda (from, to') ->
        Type.Lambda (apply_substs substs from, apply_substs substs to')
    | Type.Tuple t -> Type.Tuple (List.map t ~f:(apply_substs substs))
    | s -> s

    let apply_substs_to_substs target substs = Map.map target ~f: (fun v ->
        apply_substs substs v
    )

    let apply_substs_to_params substs params = 
        let rec apply_param param = 
            let shape = match Param.(param.shape) with
                | Param.Tuple ts -> 
                    Param.Tuple (List.map ts ~f:apply_param)
                | t -> t
            in let type' = apply_substs substs param.type' in
            Param.{type'; shape}
        in List.map params ~f:apply_param

    let apply_substs_to_scheme substs scheme = Type.{scheme with typ = apply_substs substs scheme.typ}
    (* let apply_substs_to_scheme ~ctx substs scheme = 
        {scheme with type}
        let (substs, errors) = Type.(scheme.constr)
            |> List.fold ~init:(substs, []) ~f:(fun (result, errors) (var_name) ->
            let (substs, s_errors) = combine_substs_f result (new_subst var_name (ctx.tempvar ())) in
            (substs, s_errors @ errors)
        ) in 
        flush_errors ~ctx errors;
        (empty_subst, apply_substs substs scheme.typ) *)

    (* TODO: get rid of ctx everywhere. it's a really bad idea. rly? it seems it's awesome *)
    let rec cond ~ctx env substs n =
        let unify_with = match Cond.(n.else_) with
        | Some _ -> None
        | None -> Some BaseTypes.unit
        in
        let (substs', unify_with) = List.fold Cond.(n.cases) ~init: (substs, unify_with) ~f: (fun (substs, unify_with) case' ->
            let (substs', typ) = block ~ctx env substs case'.if_ in
            let if_result = unify ~ctx BaseTypes.bool typ in
            if List.length if_result.unify_errors > 0 then begin 
                Basket.put ctx.errors (IfTypeMismatch { unexpected = typ });
            end;
            let (substs', errors) = combine_substs_f substs' if_result.unified in
            flush_errors ~ctx errors;
            let (substs'', then_) = block ~ctx env substs' case'.then_ in
            match unify_with with
            | Some t ->
                let {unified; unify_errors} = unify ~ctx t then_ in
                let (combined, errors) = combine_substs_f substs'' unified in
                flush_errors ~ctx errors;
                if List.length unify_errors > 0 then begin
                    Basket.put ctx.errors (BranchTypeMismatch {expected = t; unexpected = then_});
                    (combined, Some t)
                end else
                    (combined, Some then_)
            | None -> (substs'', Some then_)
            (* TODO: unify, not check *)
        ) in
        let unify_with' = Option.value_exn unify_with in
        match n.else_ with
        | Some else_ -> 
            let (substs'', typ) = block ~ctx env substs' else_ in
            let {unified; unify_errors} = unify ~ctx unify_with' typ in
            if List.length unify_errors > 0 then begin 
                Basket.put ctx.errors (BranchTypeMismatch {expected = unify_with'; unexpected = typ});
            end;
            let (substs'', errors) = combine_substs_f substs'' unified in
            flush_errors ~ctx errors;
            (substs'', unify_with')
        | None -> (substs', BaseTypes.unit)

    and block ~ctx env init_substs block =
        let result_stmt env substs = function
            | Stmt.Expr e -> 
                expr ~ctx env substs e 
            | _ -> raise Common.TODO
        in
        let intern_stmt env substs = function
            | Stmt.Expr e ->
                let (substs', typ) = expr ~ctx env substs e in
                begin match Type.equals typ BaseTypes.unit with
                | true -> ()
                | false -> Basket.put ctx.errors (IgnoredResult {type_provided = typ})
                end;
                (substs', env)
            | Stmt.Let _let ->
                let (substs', env') = binding ~ctx env substs _let in
                Map.iteri env' ~f:(fun ~key ~data -> Common.log ["let"; key; Type.scheme_to_string data]);
                (substs', env')
            | _ -> raise Common.TODO
        in
        let rec infer_stmt env substs = function
        | [] -> (substs, BaseTypes.unit)
        | last :: [] -> result_stmt env substs last
        | stmt :: rest -> 
            let (substs', env') = intern_stmt env substs stmt in
            infer_stmt env' substs' rest
        in
            infer_stmt env init_substs Block.(block.stmts)

    and lambda ~ctx env substs lam =
        (* here all args are either untyped or have resolved values (or errors???)*)
        (* TODO: think about it *)
        (* TODO: add *)
        (* TODO: result struct instead of this crappy tuple *)
        (* let env_with_args = Lambda.(lam.args) |> *)
        let (substs, result_type) = block ~ctx env substs Lambda.(lam.block) in
        let as_list = List.map lam.params ~f: (fun param -> param.type') in
        (* TODO: Type.lambda and Type.lambda_scheme *)
        let with_result = Type.lambda (as_list @ [result_type]) in
        Common.log ["lam"; Type.to_string result_type];
        (substs, with_result.typ)

    and ident_expr ~ctx env substs m =
        (* TODO: error vars *)
        let instantiate_scheme scheme =
            let new_substs = List.fold 
                Type.(scheme.constr) 
                ~init: substs 
                ~f: (fun substs var_name ->
                    add_subst substs var_name (ctx.tempvar())
            ) in 
            let instance = apply_substs new_substs scheme.typ in
            (* do we really need to return new substs? *)
            (new_substs, instance)
        in
        match Ident.(m.scheme) with
        | Some s -> instantiate_scheme s
        | None -> begin
            match m.resolved with
            | Local { scope_name; _ } -> begin 
                match Map.find env scope_name with
                | Some s -> 
                    instantiate_scheme s
                | None -> 
                    (* TODO: spawn error var? *)
                    raise Common.TODO
                end
            | _ -> 
                (substs, ctx.tempvar())
        end

    and binding ~ctx env substs n =
        let (substs', result_type) = block ~ctx env substs Let.(n.block) in
        (* TODO: unify with let type *)
        (* TOOD: really, how about not @ Map.mem?  *)
        let free_vars = Type.free_vars result_type
            |> Set.filter ~f: (fun name -> Map.mem substs name) 
            |> Set.to_list
        in
        let scheme = Type.make_scheme free_vars result_type in
        let env' = Map.add_exn env ~key: (n.scope_name) ~data: scheme in
        (substs', env')

    and tuple_expr ~ctx env substs t = match Tuple.(t.exprs) with
        | [] -> (substs, BaseTypes.unit)
        | single :: [] -> expr ~ctx env substs single
        | exprs ->
            let (substs', exprs) = List.fold exprs ~init: (substs, []) ~f:(fun (substs, result) node ->
                let (new_substs, node_typ) = expr ~ctx env substs node in
                let node_typ = apply_substs new_substs node_typ in
                (combine_substs ~ctx substs new_substs, node_typ :: result)
            ) in (substs', Type.Tuple (List.rev exprs))

    and apply_var ~ctx env substs v m = 
        (*todo: different oreder *)
        let eval_args nodes = stateful_map nodes ~init: substs ~f:(fun substs node ->
            let (new_substs, typ) = expr ~ctx env substs node in
            (typ, combine_substs ~ctx substs new_substs)
        ) in
        let (nodes, substs) = eval_args Apply.(m.args) in
        let result = ctx.tempvar() in
        let lam = (Type.lambda (nodes @ [result])).typ in
        let new_subst = combine_substs ~ctx substs (new_subst v lam) in
        (new_subst, result)

    and apply_lambda ~ctx env substs lam m = 
        let rec do' prev_substs = function
        | arg :: args, Type.Lambda (from, to') ->
            let (substs, node_typ) = expr ~ctx env prev_substs arg in
            let unified = unify ~ctx (apply_substs substs from) node_typ in
            begin 
                List.iter unified.unify_errors ~f:(fun err -> Basket.put ctx.errors err)
            end;
            let combined = combine_substs ~ctx unified.unified (apply_substs_to_substs substs unified.unified) in
            begin match args with
            | [] -> (combined, Some to')
            | rest -> do' combined (rest, to')
            end
        | args, t ->
            Basket.put ctx.errors @@ NotFunction {
                type_provided = t
            };
            (* possible source of bugs. check the behaviour in production *)
            let new_substs = List.fold args ~init: prev_substs ~f:(fun substs node ->
                let (new_substs, _) = expr ~ctx env substs node in
                combine_substs ~ctx substs new_substs 
            ) in
            (new_substs, Some t)
        in
        let (substs, rem) = do' substs (Apply.(m.args), Type.Lambda lam) in
        match rem with
        | Some typ -> (substs, typ)
        | None -> (substs, Type.Unknown)

    and apply_expr ~ctx env substs m =
        let (_, fn) = expr ~ctx env substs Apply.(m.fn) in
        match fn with
        | Type.Var v -> apply_var ~ctx env substs v m
        | Type.Lambda lam -> apply_lambda ~ctx env substs lam m
        | t ->
            Basket.put ctx.errors @@ NotFunction{type_provided=t};
            (empty_subst, Type.Var "")

    and expr ~ctx env substs = function
        | Expr.Value m -> (substs, Value.(m.type_))
        | Expr.Lambda lam -> lambda ~ctx env substs lam
        | Expr.Ident m -> ident_expr ~ctx env substs m
        | Expr.Tuple t -> tuple_expr ~ctx env substs t
        | Expr.Apply m -> apply_expr ~ctx env substs m
        | Expr.Cond c -> cond ~ctx env substs c

    (* /// UNDER CONSTRUCTION /// *)
    (* move into Substs module? *)
    let rec apply_substs_to_expr substs = function
    (* apply_substs_to_type *)
        | Expr.Value m -> 
            Expr.Value {m with type_ = apply_substs substs m.type_}
        | Expr.Tuple t -> 
            Expr.Tuple { exprs = List.map t.exprs ~f:(apply_substs_to_expr substs) }
        | Expr.Ident m -> 
            Expr.Ident (
                match m.scheme with
                | None -> m
                | Some scheme -> {m with scheme = Some {scheme with typ = apply_substs substs scheme.typ}}
            )
        | Expr.Apply m -> 
            let fn = apply_substs_to_expr substs m.fn in
            let args = List.map m.args ~f:(apply_substs_to_expr substs) in
            Expr.Apply {fn = fn; args = args}
        | Expr.Lambda lam -> 
            let params = apply_substs_to_params substs Lambda.(lam.params) in
            let block = apply_substs_to_block substs lam.block in
            Expr.Lambda Lambda.{params; block}
        | Expr.Cond c -> 
            let cases = List.map Cond.(c.cases) ~f:(fun cas ->
                let if_ = apply_substs_to_block substs cas.if_ in
                let then_ = apply_substs_to_block substs cas.then_ in
                Cond.{if_; then_}
            ) in
            let else_ = c.else_ |> Option.map ~f:(apply_substs_to_block substs) in
            Expr.Cond Cond.{cases; else_}

    and apply_substs_to_block substs b =  
        let stmts = List.map Block.(b.stmts) ~f: (function 
            | Stmt.Block b -> Stmt.Block (apply_substs_to_block substs b)
            | Stmt.Expr e -> Stmt.Expr (apply_substs_to_expr substs e)
            | Stmt.Let b -> 
                let scheme = Option.map b.scheme ~f:(apply_substs_to_scheme substs) in
                let block = apply_substs_to_block substs b.block in
                Stmt.Let {b with scheme; block }
        ) in Block.{stmts}

    (* === UNDER CONSTRUCTION === *)

    let toplevel_binding ~ctx n = 
        let env = (Map.empty (module String)) in
        let substs = (Map.empty (module String)) in
        let (_, result_type) = block ~ctx env substs Let.(n.block) in
        (* TODO: unify with let type *)
        let free_vars = Type.free_vars result_type
            |> Set.filter ~f: (fun name -> Map.mem substs name) 
            |> Set.to_list
        in
        let scheme = Type.make_scheme free_vars result_type in
        {n with scheme = Some scheme; block = n.block} (* TODO: unify with the resulting substs and env *)

    let modu ~ctx m = 
        let entries' = List.map Node.Module.(m.entries) ~f: (function
            | Binding n -> Node.Module.Binding (toplevel_binding ~ctx n)
        ) in
        {m with entries = entries'}
end

(* TODO: better name *)
module Global = struct 
    let binding resolver node = 
        let transformed = Transform.binding node in
        let resolve_ctx = Resolve.make_context (Resolver.Local.make resolver) in
        let (resolved, _) = Resolve.binding resolve_ctx transformed in
        let infer_ctx = Infer.make_ctx () in
        let env = Map.empty(module String) in
        let substs = Map.empty(module String) in
        let (substs', result_type) = Infer.block ~ctx:infer_ctx env substs Let.(resolved.block) in
        (* TODO: unify with let type *)
        let free_vars = Type.free_vars result_type
            |> Set.filter ~f: (fun name -> not @@ Map.mem substs name) 
            |> Set.to_list
        in
        Common.log[resolved.given_name; Type.to_string result_type; free_vars |> String.concat ~sep: ","; Infer.substs_to_string substs'];
        let block = Infer.apply_substs_to_block substs' resolved.block in
        (* TODO: rename vars to match the t, u, v, w scheme *)
        let scheme = Type.make_scheme free_vars result_type in
        let (scope_name, resolver') = Resolver.Global.add_binding resolver Let.(resolved.given_name) scheme in
        let typed_node = Let.{ resolved with
            scheme = Some scheme;
            scope_name;
            block;
        } in
        (typed_node, resolver')

    let root root_node =
        let resolver = ref @@ Resolver.Global.root () in
        let entries = List.map Ast.Node.Root.(root_node.entries) ~f: (function
            | Let b ->
                let (typed, resolver') = binding !resolver b in
                resolver := resolver';
                Module.Binding typed
        ) in
        Module.{name=""; entries}
end
