open Parlex

module Arg = struct 
    type ident = {ident_pos: Pos.t; ident: string}
    type tuple = {tuple: arg list}
    and arg = Ident of ident | Tuple of tuple
    type args = {args: arg list}
end

type int_ = {int_pos: Pos.t; int: string}
type str_ = {str_pos: Pos.t; str: string}
type ident = {ident_pos: Pos.t; ident: string}
type value = IntValue of int_ | StrValue of str_ | IdentValue of ident | LambdaValue of lambda

(* TODO: s/params/args *)
(* TODO: s/cond_expr/cond_block *)
and cond = { cond_expr: expr_or_block; cond_then: expr_or_block; cond_else: expr_or_block option }
(* TODO: better name *)
and expr_or_block = Expr of expr | Block of block
and lambda = {lambda_args: Arg.args; lambda_block: block}
and apply = { apply_fn: expr; apply_args: expr list }
and expr = ValueExpr of value | ApplyExpr of apply | CondExpr of cond
and block_stmt = ExprStmt of expr | LetStmt of let_ | BlockStmt of block
and block = {block_start_pos: Pos.t; block_end_pos: Pos.t; block_stmts: block_stmt list}
and let_expr = LetExpr of expr | LetBlock of block
and let_ = {let_pos: Pos.t; let_args: Arg.args option; let_ident: ident; let_expr: let_expr}
