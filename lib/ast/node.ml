open Common
open Base

module Import = struct
    type name = (string Span.t * kind option) 
    and kind = 
        | Rename of string Span.t
        | Names of name list

    type t = {
        keyword: string Span.t;
        name: string Span.t;
        kind: kind option;
    }

    let tree_repr node =
        let open Common.Pp in
        let import = [Pp.spanned node.keyword; spanned node.name] in
        match node.kind with
        | None -> branch import []
        | Some (Rename new_name) -> 
            branch (import @ [text "as"; spanned new_name]) []
        | Some (Names names) -> 
            let rec pp_name (name, kind) = 
                let args = [spanned name] in
                match kind with
                | None -> branch args []
                | Some (Names names) -> branch args (List.map names ~f:pp_name)
                | Some (Rename new_name) -> branch (args @ [text "as"; spanned new_name]) []
            in branch import (List.map names ~f:pp_name)
end

module Arg = struct 
    type ident = {span: Span.range; ident: string}
    type tuple = {tuple: arg list}
    and arg = 
        | Ident of ident
        | Tuple of tuple
    type args = {args: arg list}
end
module rec Int: sig 
    type t = {span: Span.range; value: string}
end = struct 
    type t = {span: Span.range; value: string}
end
and Str: sig
    type t = {span: Span.range; value: string}
end = struct 
    type t = {span: Span.range; value: string}
end
and Ident: sig
    type t = string Span.t
    val pretty_print: t -> Common.Pp.branch
end = struct 
    type t = string Span.t
    let pretty_print node = Pp.(branch [text "IDENT"; spanned node] [])

end
and Lambda: sig
    type t = {args: Arg.args; block: Block.t}
end = struct 
    type t = {args: Arg.args; block: Block.t}
end
and Let: sig
    type expr = 
        | Expr of Expr.t
        | Block of Block.t

    type t = {span: Span.range; args: Arg.args option; ident: Ident.t; expr: expr; is_rec: bool}
end = struct 
    type expr = 
        | Expr of Expr.t
        | Block of Block.t

    type t = {span: Span.range; args: Arg.args option; ident: Ident.t; expr: expr; is_rec: bool}
end
and Block: sig
    type block_stmt = 
        | Expr of Expr.t
        | Let of Let.t
        | Block of Block.t

    type t = {span: Span.range; stmts: block_stmt list}
end = struct 
    type block_stmt = 
        | Expr of Expr.t
        | Let of Let.t
        | Block of Block.t

    type t = {span: Span.range; stmts: block_stmt list}
end
and Cond: sig
    type expr = 
        | Expr of Expr.t 
        | Block of Block.t
    type t = {if_: expr; then_: expr; else_: expr option}
end = struct 
    type expr = 
        | Expr of Expr.t 
        | Block of Block.t
    type t = {if_: expr; then_: expr; else_: expr option}
end
and Apply: sig
    type t = { fn: Expr.t; args: Expr.t list }
end = struct 
    type t = { fn: Expr.t; args: Expr.t list }
end
and Value: sig
    type t = 
        | Int of Int.t 
        | Str of Str.t 
        | Ident of Ident.t 
        | Lambda of Lambda.t 
        | Unit
end = struct 
    type t = 
        | Int of Int.t
        | Str of Str.t
        | Ident of Ident.t
        | Lambda of Lambda.t
        | Unit
end
and Expr: sig
    type t = 
        | Value of Value.t
        | Apply of Apply.t
        | Cond of Cond.t
end = struct 
    type t =
        | Value of Value.t
        | Apply of Apply.t
        | Cond of Cond.t
end
and Module: sig
    type entry = Let of Let.t
end = struct 
    type entry = Let of Let.t
end

module Root = struct 
    type t = {
        entries: Module.entry list
    }
end