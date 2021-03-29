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
module Error = Error 
module Resolve = Resolve
module Transform = Transform
module Erro = Erro
module Util = Util

(* TODO: move somewhere *)
module Infer = struct 
    open Inferno
    open Erro

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
        | None -> match m.resolved.absolute with
            | None ->
                (* TODO: error node? *)
                ctx.tempvar()
            | Some {name=name; source=""; _} -> 
                begin match Map.find ctx.env (name) with
                | Some s -> 
                    instantiate_scheme s
                | None -> 
                    raise Common.TODO (* TODO: spawn error var? *)
                end
            | Some {name=_; source=_; _} -> 
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
                (* TODO: make a generic function to skip errors *)
                let errors = unify_ctx ~ctx first_t t in
                (if List.length errors > 0 then (
                    ctx_add_errors ~ctx [
                        BranchTypeMismatch {
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
        | Expr.Match m -> matc ~ctx m
        | Expr.Li l -> list ~ctx l
    and matc ~ctx m = Infer_match.matc ~ctx ~expr ~block_stmts m

end

(* TODO: better name *)
module Global = struct 
    let binding ~source resolver node = 
        let ctx = Resolve.make_context ~source resolver in
        let resolved = Resolve.toplevel_binding ctx node in
        let env = match Let.(resolved.is_rec) with
        | true -> 
            Map.add_exn (Map.empty(module String)) 
                ~key: Let.(resolved.scope_name) 
                ~data: (Option.value_exn resolved.scheme)
        | false -> Map.empty(module String)
        in
        let infer_ctx = Inferno.make_ctx ~env in
        let result_type = Infer.block ~ctx: infer_ctx Let.(resolved.block) in
        let result_type = Inferno.apply_substs ~ctx: infer_ctx result_type in
        let result_errors = Inferno.unify_ctx ~ctx: infer_ctx (resolved.result) result_type in 
        let block = Subst.apply_substs_to_block infer_ctx.substs resolved.block in

        let result_lambda = (Let.(resolved.params) 
            |> List.map ~f:(fun p -> Param.(p.type'))) 
                @ [resolved.result]
            |> Type.Lambda.make
            |> Subst.apply_substs infer_ctx.substs
        in
        let scheme = Type.make_scheme (Inferno.free_vars ~ctx: infer_ctx result_lambda) result_lambda in
        (* *)
        Resolver.Scope.set_binding_scheme resolver Let.(resolved.given_name) scheme;
            (* TODO: proper names *)
        let typed_node = Let.{ resolved with
            scheme = Some scheme;
            block;
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
        "$", Type.lambda ~constr: ["t"; "u"] [Type.Lambda (Type.Var "t", Type.Var "u"); Type.Var "t"; Type.Var "u"];
        "|>", Type.lambda ~constr: ["t"; "u"] [Type.Var "t"; Type.Lambda (Type.Var "t", Type.Var "u"); Type.Var "u"];
    ] in
    let resolver = Resolver.Scope.root source in
    List.iter predefined ~f:(fun (name, scheme) -> 
        ignore @@ Resolver.Scope.implant_binding resolver name scheme);
    (* TODO: pass name *)
    let tf = Transform.root m in 
    let m = Global.modu ~resolve_source resolver (Symbol.Id.make source [] "") Node.Module.{scope_name = ""; given_name = ""; exposed = Map.empty(module String); entries = tf.entries} in
    (match m with
    | Ok m -> Common.log["~~~"; Pp.to_string [m |> Node.Module.pretty_print]];
    | Error _ -> ());
    m