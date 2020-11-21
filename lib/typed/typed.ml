exception Unreachable
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
    type t = {name: string; type_: string}
end = struct 
    type t = {name: string; type_: string}
end
and Let: sig 
    type t = {
        ident: Ident.t;
        block: Block.t 
    }
end = struct 
    type t = {
        ident: Ident.t;
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

module Transform = struct
    open Ast

    open Common
    (* TODO: remove these transform_ prefixes *)
    (* TODO: move Int / Str to constants *)
    let args args = 
        match args with
        | [] -> [Arg.{arg = "_"}]
        | some -> some |> List.map (function
            | Node.Arg.Ident m -> Arg.{arg = m.ident}
            | Node.Arg.Tuple m -> begin
                match m.tuple with
                | [] -> Arg.{arg = "_"}
                | _ -> raise (Todo "implement tuples")
            end) 

    let rec transform_apply apply = 
        let fn = expr Node.Apply.(apply.fn) in
        let args = List.map expr Node.Apply.(apply.args) in
            Apply.{fn=fn; args=args}
    and value = function
        | Node.Value.Int v -> Expr.Value (Value.{value = v.value; type_ = Const.BasicTypes.int})
        | Node.Value.Str v -> Expr.Value (Value.{value = v.value; type_ = Const.BasicTypes.str})
        | Node.Value.Ident v -> Expr.Ident (Ident.{name = v.value; type_ = "unknown"})
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
    and block b = Block.{stmts = (List.map block_stmt b.stmts)}
    and let_expr = function
        | Node.Let.Expr v -> Block.{stmts = [Stmt.Expr (expr v)]}
        | Node.Let.Block e -> block e
    and let_ n = 
        let t_ident = Ident.{type_ = "unknown"; name = n.ident.value} in
        let t_expr = 
        match Node.Let.(n.args) with
            | Some a ->
                Block.{stmts = [
                    Stmt.Expr(Expr.Lambda(Lambda.{args = (args a.args); block = let_expr (n.expr)}))
                ]}
            | None -> let_expr Node.Let.(n.expr)
        in
        Let.{ident = t_ident; block = t_expr}
end

