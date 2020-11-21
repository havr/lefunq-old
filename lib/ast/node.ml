open Parlex

module Arg = struct 
    type ident = {ident_pos: Pos.t; ident: string}
    type tuple = {tuple: arg list}
    and arg = Ident of ident | Tuple of tuple
    type args = {args: arg list}
end
module rec Int: sig 
    type t = {pos: Pos.t; value: string}
end = struct 
    type t = {pos: Pos.t; value: string}
end
and Str: sig
    type t = {pos: Pos.t; value: string}
end = struct 
    type t = {pos: Pos.t; value: string}
end
and Ident: sig
    type t = {pos: Pos.t; value: string}
end = struct 
    type t = {pos: Pos.t; value: string}
end
and Lambda: sig
    type t = {args: Arg.args; block: Block.t}
end = struct 
    type t = {args: Arg.args; block: Block.t}
end
and Let: sig
    type expr = Expr of Expr.t | Block of Block.t
    type t = {pos: Pos.t; args: Arg.args option; ident: Ident.t; expr: expr}
end = struct 
    type expr = Expr of Expr.t | Block of Block.t
    type t = {pos: Pos.t; args: Arg.args option; ident: Ident.t; expr: expr}
end
and Block: sig
    type block_stmt = Expr of Expr.t | Let of Let.t | Block of Block.t
    type t = {start_pos: Pos.t; end_pos: Pos.t; stmts: block_stmt list}
end = struct 
    type block_stmt = Expr of Expr.t | Let of Let.t | Block of Block.t
    type t = {start_pos: Pos.t; end_pos: Pos.t; stmts: block_stmt list}
end
and Cond: sig
    type expr = Expr of Expr.t | Block of Block.t
    type t = {if_: expr; then_: expr; else_: expr option}
end = struct 
    type expr = Expr of Expr.t | Block of Block.t
    type t = {if_: expr; then_: expr; else_: expr option}
end
and Apply: sig
    type t = { fn: Expr.t; args: Expr.t list }
end = struct 
    type t = { fn: Expr.t; args: Expr.t list }
end
and Value: sig
    type t = Int of Int.t | Str of Str.t | Ident of Ident.t | Lambda of Lambda.t
end = struct 
    type t = Int of Int.t | Str of Str.t | Ident of Ident.t | Lambda of Lambda.t
end
and Expr: sig
    type t = Value of Value.t | Apply of Apply.t | Cond of Cond.t
end = struct 
    type t = Value of Value.t | Apply of Apply.t | Cond of Cond.t
end
