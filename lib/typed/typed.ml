open Base
open Common

include Node

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

module Error = struct 
    type t = 
    | UndeclaredIdentifier of {
        given_name: string;
        pos: Common.Pos.t
    } | TypeMismatch of {
        type_expected: Type.t;
        type_provided: Type.t;
    } | NotFunction of {
        type_provided: Type.t
    } | IgnoredResult of {
        unexpected: Type.t;
    } | IfTypeMismatch of {
        unexpected: Type.t
    } | BranchTypeMismatch of {
        unexpected: Type.t;
        expected: Type.t
    }

    let equals a b = match (a, b) with
    | UndeclaredIdentifier a, UndeclaredIdentifier b ->
        (phys_equal a.given_name b.given_name) && (Common.Pos.equals a.pos b.pos)
    | TypeMismatch {type_expected = te; type_provided = tp},
      TypeMismatch {type_expected = te'; type_provided = tp'} ->
        (Type.equals te te' && Type.equals tp tp')
    | IgnoredResult {unexpected=u}, IgnoredResult {unexpected=u'} -> 
        Type.equals u u'
    | NotFunction {type_provided=tp}, NotFunction {type_provided=tp'} -> 
        Type.equals tp tp'
    | IfTypeMismatch {unexpected=u}, IfTypeMismatch {unexpected=u'} -> 
        Type.equals u u'
    | BranchTypeMismatch {unexpected=u; expected=e}, BranchTypeMismatch {unexpected=u'; expected=e'} -> 
        (Type.equals u u' && Type.equals e e')
    | _ -> false

    let to_string = function 
    | UndeclaredIdentifier {given_name; _} ->
        String.concat ["Undeclared identifier: " ^ given_name]
    | TypeMismatch {type_expected = te; type_provided = tp} ->
        String.concat ["Type Mismatch: "; Type.to_string te; "!="; Type.to_string tp]
    | IgnoredResult {unexpected} -> 
        String.concat ["Ignored result: "; Type.to_string unexpected]
    | NotFunction {type_provided=tp} -> 
        String.concat ["Not a Function: "; Type.to_string tp]
    | IfTypeMismatch {unexpected=u} -> 
        String.concat ["If Type Mismtach: "; Type.to_string u]
    | BranchTypeMismatch {unexpected=u; expected=e} -> 
        String.concat ["Branch type mismtach: "; Type.to_string u; "!="; Type.to_string e]
end

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

    open Resolver

    type context = {
        (* LocalResolver? *)
        resolver: Local.t;
        errors: Error.t Basket.t;
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
        match Local.lookup ctx.resolver Node.Ident.(id.given_name) with
        | Some (`Global {global_name; scheme}) -> { id with
            resolved = Node.Ident.Global {
                global_name;
            };
            scheme = Some scheme;
        }
        | Some (`Local {scope_name; scheme}) -> { id with 
            resolved = Node.Ident.Local {
                scope_name;
                (* TODO: do we really need it here? *)
                param = false;
            };
            scheme = scheme;
        }
        | None -> 
            Basket.put ctx.errors @@ Error.UndeclaredIdentifier {given_name = id.given_name; pos = id.pos};
            id
    
    let rec binding ctx node = 
        match Let.(node.is_rec) with
        | true ->
            let scope_name = Local.add_name ctx.resolver Let.(node.given_name) None in
            (* submodule? *)
            { node with 
                block = block ctx Let.(node.block); 
                scope_name = scope_name
            }
        | false ->
            let b = block ctx Let.(node.block) in
            let scope_name = Local.add_name ctx.resolver Let.(node.given_name) None in
            { node with block = b; scope_name }

    and block ctx n =
        let stmts = List.map Block.(n.stmts) ~f:(function 
            | Stmt.Expr n -> 
                Stmt.Expr (expr ctx n)
            | Stmt.Let n -> 
                let result = (binding ctx n) in
                Stmt.Let result
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
        Common.log["type params:"; String.concat ~sep:"," @@ List.map n.params ~f: Param.to_string];
        let param_names = List.map typed_params ~f:param_name_list |> List.concat in
        let type_names = without_duplicates param_names in
        let sub = Local.sub ctx.resolver ~name: "TODO" in
        List.iter type_names
            ~f: (fun (name, scheme) -> 
                ignore @@ Local.add_name sub name (Some scheme));
        Lambda.{
            params = typed_params;
            block = block {ctx with resolver = sub} n.block
        }
end

module Transform = struct
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
        | Ast.Node.Value.Unit -> Expr.Value (Value.{value = ""; type_ = BaseTypes.unit})
        | Ast.Node.Value.Int v -> Expr.Value (Value.{value = v.value; type_ = BaseTypes.int})
        | Ast.Node.Value.Str v -> Expr.Value (Value.{value = v.value; type_ = BaseTypes.str})
        (* TODO: return AST without parlex pos *)
        | Ast.Node.Value.Ident v -> Expr.Ident (Ident.{
            given_name = v.value;
            resolved=Ident.Undefined;
            scheme = None;
            pos = Span.(v.span.start) (* TODO: keep spans! *)
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

module Subst = struct 
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
                    (result, (existing, data) :: errors)
            | None -> 
                (Map.add_exn result ~key: key ~data: data, errors)
        )

    (* more elegant solution. carry them? *)

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

    let rec apply_substs_to_expr substs = function
    (* apply_substs_to_type *)
        | Expr.Value m -> 
            Expr.Value {m with type_ = apply_substs substs m.type_}
        | Expr.Tuple t -> 
            Expr.Tuple { exprs = List.map t.exprs ~f:(apply_substs_to_expr substs) }
        | Expr.Ident m -> 
            Expr.Ident ( match m.scheme with
                | None -> m
                | Some scheme -> { m with scheme = Some { scheme with typ = apply_substs substs scheme.typ}}
            )
        | Expr.Apply m -> 
            let fn = apply_substs_to_expr substs m.fn in
            let args = List.map m.args ~f:(apply_substs_to_expr substs) in
            Expr.Apply {fn; args}
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
            let else_ = Option.map c.else_ ~f:(apply_substs_to_block substs) in
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

    let apply_to_error substs = function
    | Error.IgnoredResult {unexpected} -> Error.IgnoredResult {unexpected = apply_substs substs unexpected}
    | e -> e

end

module Infer = struct 
    type env = Type.scheme StringMap.t

    type context = {
        errors: Error.t Basket.t;
        tempvar: tempvar_gen;
        store: TypeStore.t;
    }

    (* TODO: make_context *)
    let make_ctx () = {
        errors = Basket.make ();
        tempvar = make_tempvar_gen "t";
        store = TypeStore.make ()
    }

    open Error

    let free_vars substs typ = Type.free_vars typ
        |> Set.filter ~f: (fun name -> not @@ Map.mem substs name) 
        |> Set.to_list

    let unify ta tb = 
        Common.log["SUUR UNIFY ("; Type.to_string ta; ") ~ ("; Type.to_string tb; ")"];
        let errors = ref [] in
        let rec unify' substs ta tb = 
            let append err = errors := err :: !errors in
            let mismatch () = append @@ TypeMismatch { type_expected = ta; type_provided = tb } in
            let append_combine errors = List.iter errors ~f:(
                fun (expect, got) -> append @@ TypeMismatch{type_expected = expect; type_provided = got})
            in

            match (ta, tb) with
            | (Type.Var va, _) -> Subst.new_subst va tb
            | (_, Type.Var vb) -> Subst.new_subst vb ta 
            | (Type.Simple sa, Type.Simple sb) ->
                (if not @@ String.equal sa sb then mismatch()) ;
                Subst.empty_subst
            | (Type.Tuple tua, Type.Tuple tub) ->
                if List.length tua <> List.length tub then 
                    (mismatch (); Subst.empty_subst)
                else begin 
                    List.fold (List.zip_exn tua tub) ~init: Subst.empty_subst ~f: (fun result (ia, ib) -> 
                        let unified = unify' result ia ib in 
                        let (combined, combine_errors) = Subst.combine_substs_f result unified in 
                        append_combine combine_errors;
                        combined
                    )
                end
            | (Type.Lambda (ha, ra), Type.Lambda (hb, rb)) -> begin
                let unified = unify' substs ha hb in 
                Common.log[">> LAMBDA UNIFIED"; Type.to_string ha; "~"; Type.to_string hb; "=>"; Subst.substs_to_string unified];
                let (combined, combine_errors) = Subst.combine_substs_f substs unified in 
                append_combine combine_errors;
                unify' combined (Subst.apply_substs combined ra) (Subst.apply_substs combined rb)
            end
            | _ -> 
                Common.log [Type.to_string ta; "!="; Type.to_string tb];
                mismatch ();
                Subst.empty_subst
        in (unify' (Subst.empty_subst) ta tb, !errors)

    let flush_errors ~ctx errors = List.iter errors ~f:(fun e -> Basket.put ctx.errors e)

    let type_mismatch_from = List.map ~f: (fun (expect, got) -> TypeMismatch{type_expected = expect; type_provided = got})

    let combine_substs ~ctx src dst =
        let (substs, errors) = Subst.combine_substs_f src dst in
        flush_errors ~ctx (type_mismatch_from errors);
        substs

    (* TODO: get rid of ctx everywhere. it's a really bad idea. rly? it seems it's awesome *)
    let rec cond ~ctx env substs n =
        let unify_with = match Cond.(n.else_) with
        | Some _ -> None
        | None -> Some BaseTypes.unit
        in
        let (substs', unify_with) = List.fold Cond.(n.cases) ~init: (substs, unify_with) ~f: (fun (substs, unify_with) case' ->
            let (substs', typ) = block ~ctx env substs case'.if_ in
            let (unified, unify_errors) = unify BaseTypes.bool typ in
            if List.length unify_errors > 0 then begin 
                Basket.put ctx.errors (IfTypeMismatch { unexpected = typ });
            end;
            let (substs', errors) = Subst.combine_substs_f substs' unified in
            flush_errors ~ctx (type_mismatch_from errors);
            let (substs'', then_) = block ~ctx env substs' case'.then_ in
            match unify_with with
            | Some t ->
                let (unified, unify_errors) = unify t then_ in
                let (combined, errors) = Subst.combine_substs_f substs'' unified in
                flush_errors ~ctx (type_mismatch_from errors);
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
            let (unified, unify_errors) = unify unify_with' typ in
            if List.length unify_errors > 0 then begin 
                Basket.put ctx.errors (BranchTypeMismatch {expected = unify_with'; unexpected = typ});
            end;
            let (substs'', errors) = Subst.combine_substs_f substs'' unified in
            flush_errors ~ctx (type_mismatch_from errors);
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
                let (substs', _) = expr ~ctx env substs e in
                (* TODO *)
                (* begin match Type.equals typ BaseTypes.unit with
                | true -> ()
                | false -> 
                    Common.log ["<><><>\n\n"; Type.to_string typ; Type.to_string BaseTypes.unit];
                    Basket.put ctx.errors @@ IgnoredResult {unexpected = typ}
                end; *)
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
        Common.log ["lam"; Type.to_string with_result.typ];
        (substs, with_result.typ)

    and ident_expr ~ctx env m =
        (* TODO: error vars *)
        let instantiate_scheme scheme =
            let new_substs = List.fold 
                Type.(scheme.constr) 
                ~init: Subst.empty_subst 
                ~f: (fun substs var_name ->
                    Subst.add_subst substs var_name (ctx.tempvar())
            ) in 
            Subst.apply_substs new_substs scheme.typ 
        in
        match Ident.(m.scheme) with
        | Some s -> 
            let inst = instantiate_scheme s in
            Common.log ["IDENT"; m.given_name; "{"; Type.scheme_to_string s; "} =>"; Type.to_string inst];
            inst
        | None -> match m.resolved with
            | Local { scope_name; _ } -> begin 
                match Map.find env scope_name with
                | Some s -> instantiate_scheme s
                | None -> 
                    Common.log ["Unable to find"; scope_name];
                    raise Common.TODO (* TODO: spawn error var? *)
                end
            | _ -> 
                ctx.tempvar()

    and rec_binding ~ctx env substs n =
        let result = ctx.tempvar() in
        (* two env' in same scope. wtf *)
        Common.log ["Adding"; Let.(n.scope_name)];
        let env' = Map.add_exn env ~key: Let.(n.scope_name) ~data: (Type.make_scheme [] result) in
        let (substs', result_type) = block ~ctx env' substs Let.(n.block) in
        (* TODO: unify with let type *)
        (* TOOD: really, how about not @ Map.mem?  *)
        let scheme = Type.make_scheme (free_vars substs result_type) result_type in
        let env' = Map.set env ~key: (n.scope_name) ~data: scheme in
        (substs', env')

    and binding ~ctx env substs n =
        (* TODO: unify them *)
        Common.log ["@@@"; Let.(n.given_name); n.scope_name; Bool.to_string n.is_rec];
        if Let.(n.is_rec) then rec_binding ~ctx env substs n else begin 
            let (substs', result_type) = block ~ctx env substs Let.(n.block) in
            (* TODO: unify with let type *)
            (* TOOD: really, how about not @ Map.mem?  *)
            let scheme = Type.make_scheme (free_vars substs result_type) result_type in
            let env' = Map.add_exn env ~key: (n.scope_name) ~data: scheme in
            (substs', env')
        end


    and tuple_expr ~ctx env substs t = match Tuple.(t.exprs) with
        | [] -> (substs, BaseTypes.unit)
        | single :: [] -> expr ~ctx env substs single
        | exprs ->
            let (substs', exprs) = List.fold exprs ~init: (substs, []) ~f:(fun (substs, result) node ->
                let (new_substs, node_typ) = expr ~ctx env substs node in
                let node_typ = Subst.apply_substs new_substs node_typ in
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
        let new_subst = combine_substs ~ctx substs (Subst.new_subst v lam) in
        (new_subst, result)

    and apply_lambda ~ctx env substs lam m = 
        let rec do' prev_substs = function
        | arg :: args, Type.Lambda (from, to') ->
            let (substs, node_typ) = expr ~ctx env prev_substs arg in
            let (unified, unify_errors) = unify (Subst.apply_substs substs from) node_typ in
            Common.log ["WORKING WITH LAMBDA ARG"; Type.to_string node_typ; "~"; Type.to_string from; "=>"; Subst.substs_to_string unified];
            begin 
                List.iter unify_errors ~f:(fun err -> 
                    Basket.put ctx.errors err)
            end;
            let combined = combine_substs ~ctx unified (Subst.apply_substs_to_substs substs unified) in
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
            (Subst.empty_subst, Type.Var "")

    and expr ~ctx env substs = function
        | Expr.Value m -> (substs, Value.(m.type_))
        | Expr.Lambda lam -> lambda ~ctx env substs lam
        | Expr.Ident m -> (substs, ident_expr ~ctx env m)
        | Expr.Tuple t -> tuple_expr ~ctx env substs t
        | Expr.Apply m -> apply_expr ~ctx env substs m
        | Expr.Cond c -> cond ~ctx env substs c
end

(* TODO: better name *)
module Global = struct 
    let binding resolver node = 
        let transformed = Transform.binding node in
        let resolve_ctx = Resolve.make_context (Resolver.Local.make resolver) in
        let resolved = Resolve.binding resolve_ctx transformed in

        let infer_ctx = Infer.make_ctx () in
        let substs = Map.empty(module String) in
        let tempvar = make_tempvar_gen "r" in
        let env = match Let.(resolved.is_rec) with
        | true -> 
            Map.add_exn (Map.empty(module String)) 
                ~key: Let.(resolved.scope_name) 
                ~data: (Type.make_scheme [] (tempvar()))
        | false -> Map.empty(module String)
        in
        let (substs', result_type) = Infer.block ~ctx:infer_ctx env substs Let.(resolved.block) in
        let result_type = Subst.apply_substs substs' result_type in
        let block = Subst.apply_substs_to_block substs' resolved.block in
        (* TODO: rename vars to match the t, u, v, w scheme *)
        let scheme = Type.make_scheme (Infer.free_vars substs result_type) result_type in
        let scope_name = Resolver.Global.add_binding resolver Let.(resolved.given_name) scheme in
        let typed_node = Let.{ resolved with
            scheme = Some scheme;
            scope_name;
            block;
        } in
        let resolve_errors = Basket.get resolve_ctx.errors in
        let infer_errors = Basket.get infer_ctx.errors in
        let errors = List.map (resolve_errors @ infer_errors) ~f:(Subst.apply_to_error substs') in
        (typed_node, errors)

    let root resolver root_node =
        let errors = ref [] in
        let entries = List.map Ast.Node.Root.(root_node.entries) ~f: (function
            | Let b -> 
                let (typ, err) = binding resolver b in
                errors := err @ !errors;
                Module.Binding typ;
        ) in
        (Module.{name=""; entries}, !errors)
end

let root m = 
    (* TODO: sigs? *)
    let resolver = Resolver.Global.root () in
    let int_op = Type.lambda [BaseTypes.int; BaseTypes.int; BaseTypes.int] in
    let predefined = [
        "println", Type.lambda [BaseTypes.str; BaseTypes.unit];
        "+", int_op;
        "-", int_op;
        "*", int_op;
        "/", int_op;
        ">", int_op;
        "<", int_op;
        "==", int_op;
        "!=", int_op;
        "%", int_op;
        "$", Type.lambda ~constr: ["t"; "u"] [Type.Lambda (Type.Var "t", Type.Var "u"); Type.Var "t"; Type.Var "u"];
        "|>", Type.lambda ~constr: ["t"; "u"] [Type.Var "t"; Type.Lambda (Type.Var "t", Type.Var "u"); Type.Var "u"];
    ] in
    List.iter predefined ~f:(fun (name, scheme) -> 
        ignore @@ Resolver.Global.add_binding resolver name scheme);
    Global.root resolver m