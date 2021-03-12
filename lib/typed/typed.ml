open Base
open Common

include Node
(*
REFACTOR:
 - (substs, type) stuff occurs frequently. move it to struct
 - move all stuff into separate modules
 *)

module Type = Type 
module Node = Node

let map_fst fn tuple = 
    let (fst, snd) = tuple in (fn fst, snd)

let stateful_map ~init ~f list  = 
    let (result, end_state) = List.fold list ~init: ([], init) ~f: (fun (result, state) item ->
        let (mapped, state') = f state item in
        (mapped :: result, state')
    ) in (List.rev result, end_state)
    

module Symbol = Symbol

open Util
module Resolver = Resolver
module Param = Param
module Base_types = Base_types
module Error = Error 
module Resolve = Resolve
module Transform = Transform
module Erro = Erro
module Util = Util

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



module Subst = struct 
    let empty_subst = Map.empty (module String)

    let new_subst var typ = 
        Map.add_exn empty_subst ~key: var ~data: typ 

    let add_subst substs var typ = 
        Map.add_exn substs ~key: var ~data: typ 

    let substs_to_string substs = Map.to_alist substs |> List.map ~f:(fun (k, v) ->
        (k ^ "=" ^ Type.to_string v)
    ) |> String.concat ~sep:", "

    let concat_substs base other =
        Map.fold other ~init: base ~f:(fun ~key ~data result -> Map.set result ~key ~data)

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

    let combine_substs base other =
        let applied = apply_substs_to_substs other base in
        concat_substs base applied

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
        | Expr.Li li -> Expr.Li {li with items = List.map li.items ~f: (apply_substs_to_expr substs)}
        | Expr.Foreign f -> Expr.Foreign f
        | Expr.Value m -> 
            Expr.Value {m with type_ = apply_substs substs m.type_}
        | Expr.Tuple t -> 
            Expr.Tuple { t with exprs = List.map t.exprs ~f:(apply_substs_to_expr substs) }
        | Expr.Ident m -> 
            Expr.Ident ( match m.scheme with
                | None -> m
                | Some scheme -> { m with scheme = Some { scheme with typ = apply_substs substs scheme.typ}}
            )
        | Expr.Apply m -> 
            let fn = apply_substs_to_expr substs m.fn in
            let args = List.map m.args ~f:(apply_substs_to_expr substs) in
            Expr.Apply { m with fn; args}
        | Expr.Lambda lam -> 
            let params = apply_substs_to_params substs Lambda.(lam.params) in
            let block = apply_substs_to_block substs lam.block in
            Expr.Lambda Lambda.{lam with params; block}
        | Expr.Cond c -> 
            let cases = List.map Cond.(c.cases) ~f:(fun cas ->
                let if_ = apply_substs_to_block substs cas.if_ in
                let then_ = apply_substs_to_block substs cas.then_ in
                Cond.{if_; then_}
            ) in
            let else_ = Option.map c.else_ ~f:(apply_substs_to_block substs) in
            Expr.Cond Cond.{c with cases; else_}

    and apply_substs_to_block substs b =  
        let stmts = List.map Block.(b.stmts) ~f: (function 
            | Stmt.Block b -> Stmt.Block (apply_substs_to_block substs b)
            | Stmt.Expr e -> Stmt.Expr (apply_substs_to_expr substs e)
            | Stmt.Let b -> 
                let scheme = Option.map b.scheme ~f:(apply_substs_to_scheme substs) in
                let block = apply_substs_to_block substs b.block in
                Stmt.Let {b with scheme; block }
        ) in Block.{b with stmts}

    let apply_to_error substs err = 
    match err with
    | Erro.IgnoredResult {unexpected; range} -> 
        Erro.IgnoredResult {unexpected = apply_substs substs unexpected; range}
    | Erro.TypeMismatch { type_expected; type_provided; range} -> 
    (* Common.log [
        "RNG"; Span.range_str range;
        "TE"; Type.to_string type_expected;
        "TP"; Type.to_string type_provided;
        "TE2"; Type.to_string @@ apply_substs substs type_expected;
        "TP2"; Type.to_string @@ apply_substs substs type_provided;
    ]; *)
    Erro.TypeMismatch {
        range;
        type_expected = apply_substs substs type_expected;
        type_provided = apply_substs substs type_provided;
    }
    | Erro.NotFunction { type_provided; range } -> Erro.NotFunction {
        range;
        type_provided = apply_substs substs type_provided;
    }
    | Erro.IfTypeMismatch { unexpected; range } -> Erro.IfTypeMismatch {
        range;
        unexpected = apply_substs substs unexpected;
    }
    | BranchTypeMismatch { unexpected; expected; range } -> Erro.BranchTypeMismatch {
        range;
        unexpected = apply_substs substs unexpected;
        expected = apply_substs substs expected;
    }
    | t -> t

end

module Infer = struct 
    type env = Type.scheme StringMap.t

    type ctx = {
        tempvar: tempvar_gen;
        mutable errors: Erro.t list;
        mutable env: env;
        mutable substs: Type.t StringMap.t
    }

    let make_env () = Map.empty(module String)

    (* TODO: make_context *)
    let make_ctx ~env = {
        env;
        errors = [];
        tempvar = make_tempvar_gen "t";
        substs = Map.empty (module String)
    }

    open Erro

    let free_vars ~ctx typ = Type.free_vars typ
        |> Set.filter ~f: (fun name -> not @@ Map.mem ctx.substs name) 
        |> Set.to_list


    let unify ta tb = 
        let errors = ref [] in
        let rec unify' substs ta tb = 
            let mismatch () = 
                errors := TypeMismatch {
                    type_expected = ta;
                    type_provided = tb;
                    range = Span.empty_range
                } :: !errors
            in
            match (ta, tb) with
            | (Type.Var va, _) -> 
                Subst.new_subst va tb
            | (_, Type.Var vb) -> 
                Subst.new_subst vb ta 
            | (Type.Simple _, Type.Simple _) ->
                if not @@ Type.equals ta tb then begin 
                    mismatch()
                end;
                Subst.empty_subst
            | (Type.Tuple tua, Type.Tuple tub) ->
                if List.length tua <> List.length tub then begin
                    mismatch ();
                    Subst.empty_subst
                end else begin 
                    List.fold (List.zip_exn tua tub) ~init: Subst.empty_subst ~f: (fun result (ia, ib) -> 
                        let unified = unify' result ia ib in 
                        Subst.combine_substs result unified
                    )
                end
            | (Type.Lambda (ha, ra), Type.Lambda (hb, rb)) -> begin
                let unified = unify' substs ha hb in 
                let combined = Subst.combine_substs substs unified in
                unify' combined (Subst.apply_substs combined ra) (Subst.apply_substs combined rb)
            end
            | _ -> 
                mismatch ();
                Subst.empty_subst
        in let result = unify' (Subst.empty_subst) ta tb  in
        (result, (!errors))

    let type_mismatch_from = List.map ~f: (fun (expect, got) -> 
        TypeMismatch{type_expected = expect; type_provided = got; range = Span.empty_range})


    let unify_ctx ~ctx base new' = 
        let (substs', errors) = unify base new' in
        ctx.substs <- Subst.combine_substs ctx.substs substs';
        errors

    let set_env ~ctx name scheme = 
        ctx.env <- Map.set ctx.env ~key:name ~data:scheme

    let apply_substs ~ctx typ = Subst.apply_substs ctx.substs typ

    let add_subst ~ctx v scheme = 
        ctx.substs <- Subst.combine_substs ctx.substs (Subst.new_subst v scheme) 

    let ctx_add_errors ~ctx errors = 
        List.iter errors ~f:(fun error -> ctx.errors <- ctx.errors @ [error])

    (* TODO: get rid of ctx everywhere. it's a really bad idea. rly? it seems it's awesome *)
    let rec cond ~ctx n =
        let infer_case ~ctx unify_with case' =
            let infered_if = block ~ctx Node.Cond.(case'.if_) in
            let errors = unify_ctx ~ctx Base_types.bool infered_if in
            if List.length errors > 0 then 
                ctx_add_errors ~ctx [IfTypeMismatch { range = Block.last_stmt_range case'.if_; unexpected = infered_if }];
                
            let infered_then = block ~ctx case'.then_ in
            match unify_with with
            | Some t ->
                let errors = unify_ctx ~ctx t infered_then in
                if List.length errors > 0 then begin
                    ctx_add_errors ~ctx [BranchTypeMismatch {
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
                    BranchTypeMismatch {
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

    and block ~ctx block =
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
            infer_stmt ~ctx Block.(block.stmts)

    and lambda ~ctx lam =
        (* here all args are either untyped or have resolved values (or errors???)*)
        (* TODO: add *)
        (* TODO: result struct instead of this crappy tuple *)
        (* let env_with_args = Lambda.(lam.args) |> *)
        let typ = block ~ctx Lambda.(lam.block) in
        let as_list = List.map lam.params ~f: (fun param -> param.type') in
        (* TODO: Type.lambda and Type.lambda_scheme *)
        let with_result = Type.lambda (as_list @ [typ]) in
        with_result.typ


    and ident_expr ~ctx m =
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
            instantiate_scheme s
        | None -> match m.resolved with
            | None ->
                (* TODO: error node? *)
                ctx.tempvar()
            | Some {name=name; source=""} -> 
                begin match Map.find ctx.env (name) with
                | Some s -> 
                    instantiate_scheme s
                | None -> 
                    raise Common.TODO (* TODO: spawn error var? *)
                end
            | Some {name=_; source=_} -> 
                raise Common.TODO (* TODO: spawn error var? *)

    and binding ~ctx n =
        (* TODO: unify them *)
        if Let.(n.is_rec) then 
            let result = ctx.tempvar() in
            (* two env' in same scope. wtf *)
            set_env ~ctx n.scope_name (Type.make_scheme [] result);
            let b = block ~ctx Let.(n.block) in
            (* TODO: unify with let type *)
            (* TOOD: really, how about not @ Map.mem?  *)
            set_env ~ctx n.scope_name (Type.make_scheme (free_vars ~ctx b) result);
        else begin 
            let typ = block ~ctx Let.(n.block) in
            (* TODO: unify with let type *)
            (* TOOD: really, how about not @ Map.mem?  *)
            set_env ~ctx n.scope_name 
                (Type.make_scheme (free_vars ~ctx typ) typ)
        end


    and tuple_expr ~ctx t = match Tuple.(t.exprs) with
        | [] -> Base_types.unit
        | single :: [] -> expr ~ctx single
        | exprs ->
             Type.Tuple (List.map exprs ~f:(fun node ->
             (* why do we need to apply substs? *)
                let e = expr ~ctx node in apply_substs ~ctx e
            ))

    and apply_var ~ctx v m = 
        let nodes = List.map Apply.(m.args) ~f: (expr ~ctx) in
        let result = ctx.tempvar() in
        let lam = (Type.lambda (nodes @ [result])).typ in
        add_subst ~ctx v lam;
        result;

    and apply_lambda ~ctx lam m = 
        let rec do' = function
        | arg :: args, Type.Lambda (from, to') -> begin
            let node_typ = expr ~ctx arg in
            let unify_errors = unify_ctx ~ctx (apply_substs ~ctx from) node_typ in
            if List.length unify_errors > 0 then begin
                ctx_add_errors ~ctx [
                    TypeMismatch {
                        range = Expr.range arg;
                        type_expected = from;
                        type_provided = node_typ
                    }
                ]
            end;

            match args with
            | [] -> Some to'
            | next_args -> do' (next_args, to')
        end
        | args, t ->
            ctx_add_errors ~ctx [
                NotFunction {
                    range = Span.merge 
                        (Expr.range @@ List.hd_exn args)
                        (Expr.range @@ List.last_exn args);
                    type_provided = t
                }
            ];
            (* possible source of bugs. check the behaviour in production *)
            List.iter args ~f:(fun node ->
                ignore @@ expr ~ctx node
            );
            Some t
        in
        do' (Apply.(m.args), Type.Lambda lam) 
            |> Option.value ~default: (Type.Unknown)

    and apply_expr ~ctx m =
        match expr ~ctx Apply.(m.fn) with
        | Type.Var v -> apply_var ~ctx v m
        | Type.Lambda lam -> apply_lambda ~ctx lam m
        | t ->
            let err = NotFunction{
                range = Expr.range m.fn;
                type_provided=t
            } in
            ctx_add_errors ~ctx [err];
            (Type.Var "")

    and list ~ctx n = match Li.(n.items) with
        | [] -> (Base_types.list (ctx.tempvar()))
        | single :: [] ->
            Base_types.list (expr ~ctx single)
        | first :: rest ->
            let first_t = expr ~ctx first in
            let _ = List.map rest ~f: (fun e ->
                let t = expr ~ctx e in
                let errors = unify_ctx ~ctx first_t t in
                (if List.length errors > 0 then (
                    ctx_add_errors ~ctx [
                        ListItemTypeMismatch {
                            range = Expr.range e;
                            expected = first_t;
                            unexpected = t;
                        }
                    ]
                ));
                first_t
            ) in Base_types.list (first_t)
    and expr ~ctx = function
        | Expr.Value m -> Value.(m.type_)
        | Expr.Ident m -> ident_expr ~ctx m
        | Expr.Lambda lam -> lambda ~ctx lam
        | Expr.Tuple t -> tuple_expr ~ctx t
        | Expr.Apply m -> apply_expr ~ctx m
        | Expr.Cond c -> cond ~ctx c
        | Expr.Foreign _ -> (ctx.tempvar ())
        | Expr.Li l -> Common.log["123"]; list ~ctx l
end

(* TODO: better name *)
module Global = struct 
    let binding ~source resolver node = 
        let transformed = Transform.binding node in
        let resolve_ctx = Resolve.make_context ~source (Resolver.Scope.local_root resolver) in
        let resolved = Resolve.binding resolve_ctx transformed in
        let env = match Let.(resolved.is_rec) with
        | true -> 
            Map.add_exn (Map.empty(module String)) 
                ~key: Let.(resolved.scope_name) 
                ~data: (Option.value_exn resolved.scheme)
        | false -> Map.empty(module String)
        in
        let infer_ctx = Infer.make_ctx ~env in
        let result_type = Infer.block ~ctx: infer_ctx Let.(resolved.block) in
        let result_type = Infer.apply_substs ~ctx: infer_ctx result_type in
        let result_errors = Infer.unify_ctx ~ctx: infer_ctx (resolved.result) result_type in 
        let block = Subst.apply_substs_to_block infer_ctx.substs resolved.block in

        let result_lambda = (Let.(resolved.params) 
            |> List.map ~f:(fun p -> Param.(p.type'))) 
                @ [resolved.result]
            |> Type.Lambda.make
            |> Subst.apply_substs infer_ctx.substs
        in
        let scheme = Type.make_scheme (Infer.free_vars ~ctx: infer_ctx result_lambda) result_lambda in
        (* *)
        let scope_name = Resolver.Scope.add_binding resolver Let.(resolved.given_name) scheme in
            (* TODO: proper names *)
        let typed_node = Let.{ resolved with
            scheme = Some scheme;
            scope_name = scope_name.name; (* TODO: let's use fully-qualified symbols everywhere? *)
            block;
        } in
        let resolve_errors = resolve_ctx.errors in
        let infer_errors = infer_ctx.errors in
        let errors = List.map (resolve_errors @ result_errors @ infer_errors) ~f:(Subst.apply_to_error infer_ctx.substs) in
        (typed_node, errors)
    
    let try_map list ~f =
        let rec loop result = function
        | [] -> Ok (List.rev result)
        | item :: rest ->
            match f item with
            | Ok mapped -> loop (mapped :: result) rest
            | Error e -> Error e
        in loop [] list

    let root ~resolve_source source resolver root_node =
        (* TODO: Symbol.Module / Typedef.Module / Def.Module *)
        (* TODO: Symbol.Id / some global id *)
        (* s/typedefs/typs in Symbol.t: typedef is type definition *)
        Common.log["123"];
        let entries = try_map Ast.Node.Root.(root_node.entries) ~f:(function
            | Let b -> 
                let (typ, errs) = binding ~source resolver b in
                Common.log["let it b"];
                (match errs with 
                    | [] -> Ok (Module.Binding typ)
                    | _ -> Error errs)
            | Import im -> 
                let (import, errs) = Resolve.import resolve_source resolver (Transform.import im) in
                if List.length errs > 0 then Error errs else Ok (Module.Import import)
        ) in
        match entries with
        | Error e -> Error e
        | Ok entries -> 
            let export = List.fold entries ~init:(Symbol.Module.empty (Symbol.Id.make source ""))  ~f:(fun export -> function
                | Module.Binding b -> if (String.is_prefix ~prefix:"_" b.given_name) then export else begin 
                    let internal = Symbol.Id.make source b.scope_name in
                    let exposed = Symbol.Id.make source b.given_name in
                    let def = Symbol.Binding.make ~internal exposed (Option.value ~default: Type.unknown_scheme b.scheme) in
                    {export with bindings = Map.set export.bindings ~key: b.given_name ~data: def }
                end
                | _ -> export
            ) in
            Ok (Module.{name=""; entries}, export)

end

let root ~source ~resolve_source m = 
    Common.log["213"];
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
        "$", Type.lambda ~constr: ["t"; "u"] [Type.Lambda (Type.Var "t", Type.Var "u"); Type.Var "t"; Type.Var "u"];
        "|>", Type.lambda ~constr: ["t"; "u"] [Type.Var "t"; Type.Lambda (Type.Var "t", Type.Var "u"); Type.Var "u"];
    ] in
    let resolver = Resolver.Scope.root source in
    List.iter predefined ~f:(fun (name, scheme) -> 
        ignore @@ Resolver.Scope.implant_binding resolver name scheme);
    (* TODO: pass name *)
    Global.root ~resolve_source source resolver m