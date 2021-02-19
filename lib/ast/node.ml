open Common
open Base

module Type = struct 
    type t = Simple of {
        name: string Span.t;
        args: t list;
    }
    | Lambda of {
        arg: t;
        result: t;
    } 
    | Tuple of {
        items: t list;
    }
    | Unit
end
module Import = struct 
    type name = {
        name: string Span.t;
        rename: string Span.t option;
        nested: name list option;
    }

    type t = {
        keyword: string Span.t;
        source: name;
    }

    let tree_repr node =
        let open Common.Pp in
        let rec name n = 
            let value = match n.rename with
            | None -> [spanned n.name]
            | Some rename -> [spanned n.name; text "as"; spanned rename]
            in
            let sub = match node.source.nested with
            | None -> []
            | Some names -> List.map names ~f: name
            in branch value sub
        in
        let import = match node.source.rename with
        | None -> [spanned node.keyword; spanned node.source.name]
        | Some rename -> [spanned node.keyword; spanned node.source.name; text "as"; spanned rename]
        in 
        let nested = match node.source.nested with
        | None -> []
        | Some names -> List.map names ~f: name
        in branch import nested

end
(* 
module Import = struct
    type name = (string Span.t * kind option) 
    and kind = 
        | Rename of string Span.t
        | Names of name list

    type t = {
        keyword: string Span.t;
        (* TODO(refactor): name: name *)
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
end *)

module Arg = struct 
    type ident = {span: Span.range; ident: string}
    type tuple = {tuple: arg list}
    and arg = 
        | Ident of ident
        | Tuple of tuple
    type args = {args: arg list}
end
module Unit = struct
    type t = unit Span.t
end

module rec Int: sig 
    type t = string Span.t
end = struct 
    type t = string Span.t
end
and Str: sig
    type t = string Span.t
end = struct 
    type t = string Span.t
end
and Ident: sig
    type t = string Span.t
    val pretty_print: t -> Common.Pp.branch
end = struct 
    type t = string Span.t
    let pretty_print node = Pp.(branch [text "IDENT"; spanned node] [])

end
and Lambda: sig
    type t = {
        range: Span.range;
        args: Arg.args;
        block: Block.t
    }

end = struct 
    type t = {
        range: Span.range;
        args: Arg.args;
        block: Block.t
    }
end
and Let: sig
    type expr = 
        | Expr of Expr.t
        | Block of Block.t

    type t = {
        sig': Type.t option;
        range: Span.range;
        args: Arg.args option;
        ident: Ident.t;
        expr: expr;
        is_rec: bool
    }

    val expr_range: expr -> Span.range
end = struct 
    type expr = 
        | Expr of Expr.t
        | Block of Block.t

    type t = {
        sig': Type.t option;
        range: Span.range;
        args: Arg.args option;
        ident: Ident.t;
        expr: expr;
        is_rec: bool
    }

    let expr_range = function
    | Expr e -> Expr.range e
    | Block b -> b.range
end
and Block: sig
    type block_stmt = 
        | Expr of Expr.t
        | Let of Let.t
        | Block of Block.t

    type t = {
        range: Span.range;
        stmts: block_stmt list
    }
end = struct 
    type block_stmt = 
        | Expr of Expr.t
        | Let of Let.t
        | Block of Block.t

    type t = {
        range: Span.range;
        stmts: block_stmt list
    }
end
and Cond: sig
    type expr = 
        | Expr of Expr.t 
        | Block of Block.t
    type t = {range: Span.range; if_: expr; then_: expr; else_: expr option}

    val expr_range: expr -> Span.range 
end = struct 
    type expr = 
        | Expr of Expr.t 
        | Block of Block.t
    type t = {range: Span.range; if_: expr; then_: expr; else_: expr option}

    let expr_range = function
    | Expr e -> Expr.range e
    | Block b -> b.range
end
and Apply: sig
    type t = { fn: Expr.t; args: Expr.t list; range: Span.range }
end = struct 
    type t = { fn: Expr.t; args: Expr.t list; range: Span.range }
end
and Value: sig
    type t = 
        | Int of Int.t 
        | Str of Str.t 
        | Ident of Ident.t 
        | Lambda of Lambda.t 
        | Unit of Unit.t

    val range: t -> Span.range

end = struct 
    type t = 
        | Int of Int.t
        | Str of Str.t
        | Ident of Ident.t
        | Lambda of Lambda.t
        | Unit of Unit.t

    let range = function
    | Int i -> i.range
    | Str s -> s.range
    | Ident i -> i.range
    | Lambda l -> l.range
    | Unit u -> u.range
end
and Expr: sig
    type t = 
        | Value of Value.t
        | Apply of Apply.t
        | Cond of Cond.t

    val range: Expr.t -> Span.range
end = struct 
    type t =
        | Value of Value.t
        | Apply of Apply.t
        | Cond of Cond.t

    let range = function
    | Value n -> Value.range n
    | Apply n -> n.range
    | Cond n -> n.range
end
and Module: sig
    type entry = Let of Let.t | Import of Import.t
end = struct 
    type entry = Let of Let.t | Import of Import.t
end

module Root = struct 
    type t = {
        entries: Module.entry list
    }
end