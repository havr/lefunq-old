open Base
exception Unreachable
(* TODO: Move into common *)
exception Unexpected of string 
exception Todo of string

type type_ = string

module Type = struct 
    type t = Var of string 
        | Simple of string 
        | Lambda of (t * t)
        | Tuple of (t list)

    type constr = (string)
    type scheme = {
        constr: constr list;
        typ: t
    }

    let unit = Simple "Unit"
    let unknown = Var ""

    let lambda ?(constr=[]) args = 
        let rec make_typ = function
        | [] -> raise (Unreachable)
        | typ :: [] -> typ
        | typ :: rest -> Lambda (typ, make_typ rest)
    in {typ = make_typ args; constr = constr}

    let rec to_string = function
    | Var v -> v
    | Simple v -> v
    | Tuple vs ->
        let concat = List.map vs ~f:to_string 
        |> String.concat ~sep: ", " in
        "(" ^ concat ^ ")"
    | Lambda (v, n) ->
        let vs = to_string v in
        let ns = to_string n in
        vs ^ " -> " ^ ns

    let rec equals a b = match (a, b) with
    | (Var av, Var bv) -> String.equal av bv
    | (Simple av, Simple bv) -> String.equal av bv
    | (Tuple av, Tuple bv) -> 
        if List.length av = List.length bv then 
            let count_equals = List.zip_exn av bv 
                |> List.filter ~f: (fun (av, bv) -> equals av bv) 
                |> List.length in
            count_equals = List.length av
        else false
    | (Lambda (ah, at), Lambda (bh, bt)) ->
        equals ah bh && equals at bt
    | _ -> false
end

module Arg = struct 
    type form = Tuple of (form list) | Name of {
        name: string; 
        type_: Type.t option
    }

    let rec names = function
    | Tuple ns -> List.map ns ~f:names |> List.concat
    | Name n -> [n.name]

    type t = { form: form; type_: Type.t }
    type args = t list
end

module rec Cond: sig 
    type case = {
        if_: Block.t;
        then_: Block.t
    }
    and t = {
        cases: case list;
        else_: Block.t option;
    }
end = struct 
    type case = {
        if_: Block.t;
        then_: Block.t
    }
    and t = {
        cases: case list;
        else_: Block.t option;
    }
end
and Apply: sig
    type t = { fn: Expr.t; args: Expr.t list }
end = struct 
    type t = { fn: Expr.t; args: Expr.t list }
end
and Value: sig
    type t = {value: string; type_: string}
end = struct 
    type t = {value: string; type_: string}
end
and Ident: sig 
(* 
given_name: foo
scope_name: d.2.1.foo.quux.foo
can be:
    - local (go.go'.quux)
    - global (Async.go)
can have
    - type

Ident can resolve to:
 - value (map, fold, names), field
 - type variable (t, u, quux)
 - module
 - type, class or instance (List, Int, Monad)

mod mod mod name field
// Global identifier: resolve the toppest (Std), then go down
Std.Foo.Bar.baz.quux
// Local identifier:
let foo x = {
    let b = 1
    let b = 2
    let inner quux = {
        let another_inner = {
            let b = b + 1
            let b = hello
            let one more inner = {
                let this = b
                // given_name b
                // scope_name (local) inner.another_inner.b$2
                let that = Std.Out.handle
                // (full, global) Std.Out.handle
                let param = quux
                // param quux, scope_name: inner.(param) quux
            }
        }
    }
}
*)
    type resolved = Local of {
        scope_name: string;
        param: bool
    } | Global of {
        global_name: string;
    } | Undefined

    type t = {
        pos: Common.Pos.t;
        given_name: string; 
        resolved: resolved;
        (* TODO(refactor): type -> scheme *)
        type_: Type.scheme;
    }
end = struct 
    type resolved = Local of {
        scope_name: string;
        param: bool
    } | Global of {
        global_name: string;
    } | Undefined

    type t = {
        pos: Common.Pos.t;
        given_name: string; 
        resolved: resolved;
        type_: Type.scheme;
    }
end
and Let: sig 
    type t = {
        name: string;
        is_rec: bool;
        block: Block.t 
    }
end = struct 
    type t = {
        name: string;
        is_rec: bool;
        block: Block.t 
    }
end
and Stmt: sig 
    type t = Expr of Expr.t | Let of Let.t | Block of Block.t
end = struct 
    type t = Expr of Expr.t | Let of Let.t | Block of Block.t
end
and Block: sig 
    type t = {stmts: Stmt.t list}
end = struct 
    type t = {stmts: Stmt.t list}
end
and Tuple: sig 
    type t = {exprs: Expr.t list}
end = struct 
    type t = {exprs: Expr.t list}
end
and Lambda: sig 
    type t = {
        args: Arg.args;
        block: Block.t;
    }
end = struct 
    type t = {
        args: Arg.args;
        block: Block.t;
    }
end
and Expr: sig 
    type t = Value of Value.t | Ident of Ident.t | Apply of Apply.t | Lambda of Lambda.t | Cond of Cond.t | Tuple of Tuple.t
end = struct 
    type t = Value of Value.t | Ident of Ident.t | Apply of Apply.t | Lambda of Lambda.t | Cond of Cond.t | Tuple of Tuple.t
end

(*
lambda args Destruct: [Type]
let foo x (y, m): (Int, Int) (y, m): (TupleType)

arg has name, type
(name, foobar): MyType
let foo &{name, surname, foo: &{x}}: MyType = {
    foo name
}

*)

module Destructured = struct 
end

module TypeNamer = struct 
    type t = {
        mutable idx: int
    }

    let make () = {idx = 0}
    let next namer = 
        let idx = namer.idx in
        namer.idx <- namer.idx + 1;
        "t" ^ (Int.to_string idx)
end

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

module Resolve = struct
    type error = { given_name: string; pos: Common.Pos.t }

    type context = {
        resolver: Resolver.t;
        errors: error Basket.t;
    }

    let make_context resolver = {
        resolver = resolver;
        errors = Basket.make();
    }

    let with_sub_resolver ctx name = {
        ctx with resolver = Resolver.sub ~name: name ctx.resolver
    }

    let ident ctx id = 
        let resolution = Resolver.lookup ctx.resolver Ident.(id.given_name) in
        match resolution with
        | Some name -> 
            (* TODO: global ids *)
            Ident.{id with 
                resolved = Ident.Local {
                    scope_name = name;
                    param = false;
                }
            }
        | None -> 
            Basket.put ctx.errors {given_name = id.given_name; pos = id.pos};
            id
    
    let rec let_ ctx n = 
        let with_name = Resolver.add_name ctx.resolver Let.(n.name) in
        let use_resolver = if n.is_rec then {ctx with resolver = with_name} else ctx in
        ({n with block = block use_resolver Let.(n.block)}, {ctx with resolver = with_name})
    and block ctx n =
        let (stmts, _) = stateful_map Block.(n.stmts) ~init: ctx ~f:(fun ctx node ->
            match node with
            | Stmt.Expr n -> 
                (Stmt.Expr (expr ctx n), ctx)
            | Stmt.Let n -> 
                let (result, new_ctx) = (let_ ctx n) in
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
        let lambda_ctx = List.fold Lambda.(n.args) 
        ~init: (with_sub_resolver ctx "TODO") 
        ~f: (fun ctx arg ->
            let resolver = Arg.(arg.form) |> Arg.names |> List.fold ~init: ctx.resolver ~f:(fun resolver name ->
                Resolver.add_name resolver name
            ) in
            {ctx with resolver = resolver}
        ) in Lambda.{ args = Lambda.(n.args); block = block lambda_ctx Lambda.(n.block) }
end

module Transform = struct
    open Ast

    open Common
    (* TODO: remove these transform_ prefixes *)
    (* TODO: move Int / Str to constants *)
    let args args = 
        let rec map_arg = function
        | Node.Arg.Ident m -> 
            Arg.Name{name = m.ident; type_ = None}
        | Node.Arg.Tuple m -> begin
            (* TODO: use unit here *)
            match m.tuple with
            | [] -> 
                Arg.Tuple []
            | name :: [] -> map_arg name
            | tuple ->
                Arg.Tuple (List.map tuple ~f:map_arg)
        end in List.map args ~f:(fun m -> 
            Arg.{form = map_arg m; type_ = Type.unknown}
        )

    let rec transform_apply apply = 
        let fn = expr Node.Apply.(apply.fn) in
        let args = List.map ~f:expr Node.Apply.(apply.args) in
            Apply.{fn=fn; args=args}
    and value = function
        | Node.Value.Int v -> Expr.Value (Value.{value = v.value; type_ = Const.BasicTypes.int})
        | Node.Value.Str v -> Expr.Value (Value.{value = v.value; type_ = Const.BasicTypes.str})
        (* TODO: return AST without parlex pos *)
        | Node.Value.Ident v -> Expr.Ident (Ident.{
            given_name = v.value;
            resolved=Ident.Undefined;
            type_ = {typ = Type.Var ""; constr = []};
            pos = v.pos |> Ast.from_parlex_pos
        })
        | Node.Value.Lambda v -> 
            let b = block v.block in
            let a = args v.args.args in
                Expr.Lambda (Lambda.{args = a; block = b})
        
    and cond n = 
        let expr_or_block = function
            (* TODO: variable names *)
            | Node.Cond.Expr e -> Block.{stmts = [Stmt.Expr (expr e)]}
            | Node.Cond.Block b -> block b
        in
        let case = Cond.{if_ = expr_or_block Node.Cond.(n.if_); then_ = expr_or_block Node.Cond.(n.then_)} in
        match n.else_ with
        | None ->
            Cond.{cases = [case]; else_ = None}
        | Some (Node.Cond.Expr (Node.Expr.Cond e)) ->
                let result = cond e in 
                {result with cases = case :: result.cases}
        | Some m -> Cond.{cases = [case]; else_ = Some (expr_or_block m)}
    and expr expr = 
        let expr' = match expr with
            | Node.Expr.Value v -> value v
            | Node.Expr.Apply app -> Expr.Apply (transform_apply app)
            | Node.Expr.Cond n -> Expr.Cond (cond n)
        in
            expr'
    and block_stmt = function
        | Node.Block.Expr e -> Stmt.Expr (expr e)
        | Node.Block.Block b -> Stmt.Block (block b)
        | Node.Block.Let t -> Stmt.Let (let_ t)
    and block b = Block.{stmts = (List.map ~f:block_stmt b.stmts)}
    and let_expr = function
        | Node.Let.Expr v -> Block.{stmts = [Stmt.Expr (expr v)]}
        | Node.Let.Block e -> block e
    and let_ n = 
        let t_expr = match Node.Let.(n.args) with
            | Some a ->
                Block.{stmts = [
                    Stmt.Expr(Expr.Lambda(Lambda.{args = (args a.args); block = let_expr (n.expr)}))
                ]}
            | None -> let_expr Node.Let.(n.expr)
        in
        Let.{name = n.ident.value ; block = t_expr; is_rec = n.is_rec}
end

(* TODO: move somewhere *)
let root root_stmts = 
    (* TODO: sigs? *)
    let resolver = ["println"; "+"; "-"; "*"; "/"; ">"; "<"; "$"; "|>"; "=="; "!="; "%"] 
        |> List.fold ~init:(Resolver.local()) ~f:(Resolver.add_name) in
    let root_ctx = Resolve.make_context resolver in
    root_stmts |> stateful_map ~init:root_ctx  ~f:(fun ctx stmt ->
    (* let's make it work with let_ for now *)
        let typed_let = Transform.let_ stmt in
        let (resolved, ctx') = Resolve.let_ ctx typed_let in
        (resolved, ctx')
    ) 

module SymbolTypeMap = Map.M(String)

module StringMap = Map.M(String)

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

(* Merge with Infer*)
module Unify = struct 
    let empty_subst = Map.empty (module String)

    let new_subst var typ = 
        Map.add_exn empty_subst ~key: var ~data: typ 

    type error = Mismatch of {
        expected: Type.t;
        got: Type.t
    }
    type context = {
        errors: error Basket.t;
    }

    let merge ~ctx src dst =
        Map.fold dst ~init: src ~f: (fun ~key ~data result ->
            match Map.find result key with
            | Some existing -> 
                if (Type.equals existing data) then
                    result
                    (*Map.add_exn result ~key: key ~data: data*)
                else 
                    begin Basket.put ctx.errors (Mismatch {
                        expected = existing;
                        got = data
                    });
                    result end
            | None -> 
                Map.add_exn result ~key: key ~data: data
        )

    let error_equals ea eb = match (ea, eb) with
    | (Mismatch {expected = e1; got = g1}, Mismatch {expected = e2; got = g2}) ->
        Type.equals e1 e2 && Type.equals g1 g2

    let make_ctx () = {errors = Basket.make ()}

    let rec unify ~ctx ta tb = match (ta, tb) with
    | (Type.Var va, _) -> new_subst va tb
    | (_, Type.Var vb) -> new_subst vb ta
    | (Type.Simple sa, Type.Simple sb) ->
        begin if not @@ String.equal sa sb then Basket.put ctx.errors @@ Mismatch {
            expected = ta;
            got = tb
        } end;
        empty_subst
    | (Type.Tuple tua, Type.Tuple tub) ->
        (if List.length tua <> List.length tub then raise Common.TODO);
        List.fold (List.zip_exn tua tub) ~init: (empty_subst)
            ~f: (fun subst (ia, ib) -> merge ~ctx subst (unify ~ctx ia ib))
    | (Type.Lambda la, Type.Lambda lb) -> begin
        let lau = unroll_lambda la in
        let lbu = unroll_lambda lb in
        (if List.length lau <> List.length lbu then raise Common.TODO);
        List.fold (List.zip_exn lau lbu) ~init: (empty_subst)
            ~f: (fun subst (ia, ib) -> merge ~ctx subst (unify ~ctx ia ib))
    end
    | _ -> raise Common.TODO
end

module Infer = struct 
    type error = TypeMismatch of {
        type_expected: Type.t;
        type_provided: Type.t;
    } | NotFunction of {
        type_provided: Type.t
    }

    let error_equals a b = match (a, b) with
    | TypeMismatch {type_expected = te; type_provided = tp},
      TypeMismatch {type_expected = te'; type_provided = tp'} ->
        (Type.equals te te' && Type.equals tp tp')
    | NotFunction {type_provided= tg}, NotFunction {type_provided=tg'} -> 
        Type.equals tg tg'
    | _ -> false
    type context = {
        errors: error Basket.t;
        namer: TypeNamer.t;
        store: TypeStore.t;
    }
    let empty_subst = Map.empty (module String)

    let new_subst var typ = 
        Map.add_exn empty_subst ~key: var ~data: typ 

    let substs_to_string substs = Map.to_alist substs |> List.map ~f:(fun (k, v) ->
        (k ^ "=" ^ Type.to_string v)
    ) |> String.concat ~sep:", "

    let combine_substs ~ctx src dst =
        Map.fold dst ~init: src ~f: (fun ~key ~data result ->
            match Map.find result key with
            | Some existing -> 
                if (Type.equals existing data) then
                    result
                else 
                    (* really? when could it appear? *)
                    begin 
                    Basket.put ctx.errors @@ TypeMismatch {
                        type_expected=existing;
                        type_provided=data
                    };
                    result end
            | None -> 
                Map.add_exn result ~key: key ~data: data
        )

    (* ta should be smth like template, tb is the stuff being unified*)
    let rec unify ~ctx ta tb = match (ta, tb) with
    | (Type.Var va, _) -> new_subst va tb
    | (_, Type.Var vb) -> new_subst vb ta
    | (Type.Simple sa, Type.Simple sb) ->
        begin 
            if not @@ String.equal sa sb then begin
                Basket.put ctx.errors @@ TypeMismatch {
                    type_expected = ta;
                    type_provided = tb
                }
            end;
        end;
        empty_subst
    | (Type.Tuple tua, Type.Tuple tub) ->
        (if List.length tua <> List.length tub then raise Common.TODO);
        List.fold (List.zip_exn tua tub) ~init: (empty_subst)
            ~f: (fun subst (ia, ib) -> combine_substs ~ctx subst (unify ~ctx ia ib))
    | (Type.Lambda la, Type.Lambda lb) -> begin
        let lau = unroll_lambda la in
        let lbu = unroll_lambda lb in
        (if List.length lau <> List.length lbu then raise Common.TODO);
        List.fold (List.zip_exn lau lbu) ~init: (empty_subst)
            ~f: (fun subst (ia, ib) -> combine_substs ~ctx subst (unify ~ctx ia ib))
    end
    | _ -> 
        raise Common.TODO

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
    
    let apply_substs_to_scheme ~ctx substs scheme = let substs = Type.(scheme.constr) |> List.fold ~init:substs ~f:(fun result (var_name) ->
        combine_substs ~ctx result (new_subst var_name (Type.Var (TypeNamer.next ctx.namer)))
    ) in (Unify.empty_subst, apply_substs substs scheme.typ)

    let rec lambda _ _ = raise Common.TODO
    and block _ _ = raise Common.TODO
    and stmt _ _ = raise Common.TODO
    and expr ~ctx substs n =
        match n with
        | Expr.Value m -> 
            (substs, Type.Simple Value.(m.type_))
        | Expr.Ident m ->
            (* refactor: move it from unify *)
            let (new_substs, t) = apply_substs_to_scheme ~ctx substs m.type_ in
            (combine_substs ~ctx substs new_substs, t)
        | Expr.Tuple t -> 
            let (subst, exprs) = List.fold Tuple.(t.exprs) ~init: (substs, []) ~f:(fun (substs, result) node ->
                let (new_substs, node_typ) = expr ~ctx substs node in
                let node_typ = apply_substs new_substs node_typ in
                (combine_substs ~ctx substs new_substs, node_typ :: result)
            ) in (subst, Type.Tuple (List.rev exprs))
        | Expr.Apply m -> 
            let eval_args nodes = stateful_map nodes ~init: substs ~f:(fun substs node ->
                let (new_substs, typ) = expr ~ctx substs node in
                (typ, combine_substs ~ctx substs new_substs)
            ) in
            let (_, fn) = expr ~ctx substs m.fn in
            begin match fn with
            | Type.Var v ->
                (*todo: different oreder *)
                let (nodes, substs) = eval_args m.args in
                let result = Type.Var (TypeNamer.next ctx.namer) in
                let lam = (Type.lambda (nodes @ [result])).typ in
                let new_subst = combine_substs ~ctx substs (new_subst v lam) in
                (new_subst, result)
            | Type.Lambda lam ->
                let rec do' prev_substs = function
                | arg :: args, Type.Lambda (from, to') ->
                    let (substs, node_typ) = expr ~ctx prev_substs arg in
                    let unified = unify ~ctx (apply_substs substs from) node_typ in
                    let combined = combine_substs ~ctx unified (apply_substs_to_substs substs unified) in
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
                        let (new_substs, _) = expr ~ctx substs node in
                        combine_substs ~ctx substs new_substs 
                    ) in
                    (new_substs, Some t)
                in
                let (substs, rem) = do' substs (m.args, Type.Lambda lam) in
                begin match rem with
                | Some typ -> (substs, typ)
                | None -> (substs, Type.Var "") end
            | t ->
                Basket.put ctx.errors @@ NotFunction{type_provided=t};
                (Unify.empty_subst, Type.Var "")
            end
        | _ -> raise (Todo "WIP: Unimplemened")
end
module Constraint = struct

end
