exception Unreachable
exception Todo of string

type type_ = string

(* type tpe = 
    | Value of string
    | Lambda of (tpe * tpe)
    | Tuple of (tpe list) *)

type int_value = {int_value: string}
type str_value= {str_value: string}

type ident_value = {ident_type: string; local_name: string}

type basic = {basic_value: string; basic_type: string}



module Elem = struct 
    module Arg = struct 
        type arg = { arg: string }
        type args = arg list
    end

    type apply = {
        apply_fn: expr;
        apply_args: expr list
    }
    (* TODO: collapse if-else's (as a part of optimization?) *)
    and cond = {
        cond_block: block;
        cond_then: block;
        cond_else: block option;
    }
    and expr = BasicExpr of basic | IdentExpr of ident_value | ApplyExpr of apply | LambdaExpr of lambda | CondExpr of cond
    and let_ = {
        let_ident: ident_value;
        let_block: block 
    }
    and block_stmt = ExprStmt of expr | LetStmt of let_ | BlockStmt of block
    and block = { block_stmts: block_stmt list }
    and lambda = {
        lambda_args: Arg.args;
        lambda_block: block;
    }
end

module Transform = struct
    open Ast

    open Elem
    open Common
    (* TODO: remove these transform_ prefixes *)
    (* TODO: move Int / Str to constants *)
    let args args = 
        match args with
        | [] -> [Elem.Arg.{arg = "_"}]
        | some -> some |> List.map (function
            | Node.Arg.Ident m -> Elem.Arg.{arg = m.ident}
            | Node.Arg.Tuple m -> begin
                match m.tuple with
                | [] -> Elem.Arg.{arg = "_"}
                | _ -> raise (Todo "implement tuples")
            end) 

    let rec transform_apply apply = 
        let fn = expr Node.(apply.apply_fn) in
        let args = List.map expr Node.(apply.apply_args) in
            {apply_fn=fn; apply_args=args}
    and value = function
        | Node.IntValue v -> BasicExpr {basic_value = v.int; basic_type = Const.BasicTypes.int}
        | Node.StrValue v -> BasicExpr {basic_value = v.str; basic_type = Const.BasicTypes.str}
        | Node.IdentValue v -> IdentExpr {local_name = v.ident; ident_type = "unknown"}
        | Node.LambdaValue v -> 
            let b = block v.lambda_block in
            let a = args v.lambda_args.args in
                LambdaExpr(Elem.{lambda_args = a; lambda_block = b})
        
    and cond n = 
        let expr_or_block = function
            (* TODO: variable names *)
            | Node.Expr e -> {block_stmts = [ExprStmt (expr e)]}
            | Node.Block b -> block b
        in
        let if_block  = expr_or_block Node.(n.cond_expr) in
        let then_block = expr_or_block n.cond_then in
        let else_block = n.cond_else |> Option.map expr_or_block in
            Elem.{cond_block = if_block; cond_then = then_block; cond_else = else_block}

    and expr expr = 
        let expr' = match expr with
            | Node.ValueExpr v -> value v
            | Node.ApplyExpr app -> ApplyExpr (transform_apply app)
            | Node.CondExpr n -> CondExpr (cond n)
        in
            expr'
    and block_stmt = function
        | Node.ExprStmt e -> ExprStmt (expr e)
        | Node.BlockStmt b -> BlockStmt (block b)
        | Node.LetStmt t -> LetStmt (let_ t)
    and block b = {block_stmts = (List.map block_stmt b.block_stmts)}
    and let_expr (ast_block: Node.let_expr) = 
        let open Elem in 
        match ast_block with
        | Node.LetExpr v -> {block_stmts = [ExprStmt (expr v)]}
        | Node.LetBlock e -> block e
    and let_ let_ = 
        let t_ident = {ident_type = "unknown"; local_name = let_.let_ident.ident} in
        let t_expr = 
        match let_.let_args with
            | Some a -> {
                block_stmts = [
                    ExprStmt(
                        LambdaExpr(Elem.{lambda_args = (args a.args); lambda_block = let_expr(let_.let_expr)})
                    )]
                }
            | None -> let_expr Node.(let_.let_expr)
        in
        {let_ident = t_ident; let_block = t_expr}
end

