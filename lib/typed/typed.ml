open Base
exception Unreachable
(* TODO: Move into common *)
exception Unexpected of string 
exception Todo of string

type type_ = string

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
        global_name: string
    } | Undefined

    type t = {
        pos: Common.Pos.t;
        given_name: string; 
        resolved: resolved;
        type_: string;
    }
end = struct 
    type resolved = Local of {
        scope_name: string;
        param: bool
    } | Global of {
        global_name: string
    } | Undefined

    type t = {
        pos: Common.Pos.t;
        given_name: string; 
        resolved: resolved;
        type_: string;
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
    type t = Value of Value.t | Ident of Ident.t | Apply of Apply.t | Lambda of Lambda.t | Cond of Cond.t
end = struct 
    type t = Value of Value.t | Ident of Ident.t | Apply of Apply.t | Lambda of Lambda.t | Cond of Cond.t
end

and Arg: sig 
    type t = { arg: string }
    type args = t list
end = struct 
    type t = { arg: string }
    type args = t list
end

module Resolve = struct
    type error = { given_name: string; pos: Common.Pos.t }

    type 't result = ('t * error list)

    let ident resolver id = match Resolver.lookup resolver Ident.(id.given_name) with
        | Some name -> 
            (Ident.{id with resolved = Ident.Local {
                scope_name = name;
                param = false
            }}, [])
        | None -> (id, [{given_name = id.given_name; pos = id.pos}])
    
    let rec let_ resolver n = 
        let with_name = Resolver.add_name resolver Let.(n.name) in
        let use_resolver = if n.is_rec then with_name else resolver in
        let (block_r, block_err) = block use_resolver Let.(n.block) in
        (Let.{
            name = n.name;
            is_rec = n.is_rec;
            block = block_r;
        }, with_name, block_err)
    and block resolver n =
        let rec block' (stmts, errors, resolver) = function
        | [] -> (List.rev stmts, errors)
        | stmt :: rest -> 
            match stmt with
            | Stmt.Expr n -> 
                let (node, e) = (expr resolver n)  in
                block' (Stmt.Expr node :: stmts, e @ errors, resolver) rest
            | Stmt.Let n -> 
                let (node, new_resolver, e) = (let_ resolver n) in
                block' ((Stmt.Let node) :: stmts, e @ errors, new_resolver) rest
            | Stmt.Block _ -> raise (Unexpected"disallow blocks inside blocks")
        in let (result, errors) = block' ([], [], resolver) n.stmts
        in (Block.{stmts = result}, errors)
    and expr resolver = function
    | Expr.Value v -> (Expr.Value v, [])
    | Expr.Ident n -> 
        let (node, errors) = ident resolver n in 
        (Expr.Ident (node), errors)
    | Expr.Apply n -> 
        let (node, errors) = apply resolver n in 
        (Expr.Apply node, errors)
    | Expr.Lambda n -> 
        let (node, errors) = lambda resolver n in 
        (Expr.Lambda node, errors)
    | Expr.Cond n -> 
        let (node, errors) = cond resolver n in 
        (Expr.Cond (node), errors)
    and apply resolver n = 
        let (fn, fn_e) = expr resolver Apply.(n.fn) in
        let (args, args_e) = List.unzip (List.map ~f: (expr resolver) Apply.(n.args)) in
        (Apply.{
            fn = fn;
            args = args;
        }, fn_e @ (List.concat args_e)) 
    and cond resolver n = 
        let (cases, cases_e) = List.map n.cases ~f:(fun {if_; then_} ->
            let (if_r, if_err) = block (Resolver.sub ~name: "TODO" resolver) if_ in
            let (then_r, then_err) = block (Resolver.sub ~name: "TODO" resolver) then_ in
            (Cond.{if_ = if_r; then_ = then_r}, if_err @ then_err)
        ) |> List.unzip in
        let else_ = Cond.(n.else_) |> Option.map ~f: (block (Resolver.sub ~name: "TODO" resolver)) in
        let else_block = else_ |> Option.map ~f: (fun(value, _) -> value) in
        let else_errs = else_ |> Option.map ~f: (fun(_, value) -> value) |> Option.value ~default: [] in
        (Cond.{
            cases = cases; 
            else_ = else_block
        }, List.concat cases_e @ else_errs)
    and lambda resolver n = 
        let with_arguments = List.fold Lambda.(n.args) ~init: (Resolver.sub ~name: "TODO" resolver) ~f: (fun resolver arg ->
            Resolver.add_name resolver Arg.(arg.arg)
        ) in
        let (block_r, block_e) = block with_arguments Lambda.(n.block) in
        (Lambda.{
            args = Lambda.(n.args);
            block = block_r
        }, block_e)

end

module Transform = struct
    open Ast

    open Common
    (* TODO: remove these transform_ prefixes *)
    (* TODO: move Int / Str to constants *)
    let args args = 
        match args with
        | [] -> [Arg.{arg = "_"}]
        | some -> some |> List.map ~f:(function
            | Node.Arg.Ident m -> Arg.{arg = m.ident}
            | Node.Arg.Tuple m -> begin
                match m.tuple with
                | [] -> Arg.{arg = "_"}
                | _ -> raise (Todo "implement tuples")
            end) 

    let rec transform_apply apply = 
        let fn = expr Node.Apply.(apply.fn) in
        let args = List.map ~f:expr Node.Apply.(apply.args) in
            Apply.{fn=fn; args=args}
    and value = function
        | Node.Value.Int v -> Expr.Value (Value.{value = v.value; type_ = Const.BasicTypes.int})
        | Node.Value.Str v -> Expr.Value (Value.{value = v.value; type_ = Const.BasicTypes.str})
        (* TODO: return AST without parlex pos *)
        | Node.Value.Ident v -> Expr.Ident (Ident.{given_name = v.value; resolved=Ident.Undefined; type_ = ""; pos = v.pos |> Ast.from_parlex_pos})
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
    let resolver = ["println"; "+"; "-"; "*"; "/"; ">"; "<"; "$"; "|>"; "=="] |> List.fold ~init:(Resolver.local()) ~f:(Resolver.add_name) in
    root_stmts |> List.fold ~init: (resolver, [], []) ~f:(fun (resolver, errors, result) stmt ->
    (* let's make it work with let_ for now *)
    let typed_let = Transform.let_ stmt in
    let (resolved, updated_resolver, errors') = Resolve.let_ resolver typed_let in
    (updated_resolver, errors @ errors', resolved :: result)
)
