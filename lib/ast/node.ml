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

    let simple name args = Simple {name; args}

    let rec pretty_print = function
        | Simple t -> Pp.(branch [text "TYPE"; spanned t.name] (List.map ~f: pretty_print t.args))
        | Lambda la -> Pp.(branch [text "LAMBDA"] [pretty_print la.arg; pretty_print la.result])
        | Tuple t -> Pp.(branch [text "TUPLE"] (List.map t.items ~f: pretty_print))
        | Unit -> Pp.(branch [text "()"] [])
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

    let pretty_print node =
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

(* module Arg = struct 
    type ident = {span: Span.range; ident: string}
    type tuple = {tuple: arg list}
    and arg = 
        | Ident of ident
        | Tuple of tuple
    type args = {args: arg list}

    let rec pretty_print_arg = function
        | Ident id -> Pp.(branch [text "IDENT"; text id.ident] [])
        | Tuple tup -> Pp.(branch [text "TUPLE"] (List.map ~f:pretty_print_arg tup.tuple))
end *)
module Unit = struct
    type t = unit Span.t
    let pretty_print _ = Pp.(branch [text "()"] [])
end

module rec Dummy: sig end = struct end
and Param: sig 
    type shape = 
        | Ident of string Span.t
        | Tuple of shape list

    type t = 
        | Positional of {
            shape: shape; 
            typ: Type.t option
        }
        | Named of {
            name: string Span.t;
            typ: Type.t option
        }
        | Optional of {
            name: string Span.t;
            typ: Type.t option;
            expr: Expr.t option
        }
        | Extension of {
            name: string Span.t;
            typ: Type.t;
        }

    val pretty_print: t -> Pp.branch
    val pretty_print_positional: shape -> Pp.branch
end = struct
    type shape = 
        | Ident of string Span.t
        | Tuple of shape list
    type t = 
        | Positional of {
            shape: shape; 
            typ: Type.t option
        }
        | Named of {
            name: string Span.t;
            typ: Type.t option
        }
        | Optional of {
            name: string Span.t;
            typ: Type.t option;
            (* TODO: default *)
            expr: Expr.t option
        }
        | Extension of {
            name: string Span.t;
            typ: Type.t;
        }

    let rec pretty_print_positional = function
        | Ident id -> Pp.(branch [text "IDENT"; spanned id] [])
        | Tuple tup -> Pp.(branch [text "TUPLE"] (List.map ~f:pretty_print_positional tup))
    
    let pretty_print = function
        | Positional p -> 
            let typ = match p.typ with
                | None -> []
                | Some m -> [Type.pretty_print m]
            in
            Pp.(branch [text "param"] (typ @ [pretty_print_positional p.shape]))
        | Named p -> 
            let typ = match p.typ with
                | None -> []
                | Some m -> [Type.pretty_print m]
            in
            Pp.(branch [text "&"; spanned p.name] typ)
        | Optional p ->
            let typ = match p.typ with
                | None -> []
                | Some m -> [Type.pretty_print m]
            in
            let expr = match p.expr with
                | None -> []
                | Some m -> [Expr.pretty_print m]
            in
            Pp.(branch [text "?"; spanned p.name] (typ @ expr))
        | Extension p ->
            Pp.(branch [text "&.."; spanned p.name] [Type.pretty_print p.typ])
end and Int: sig 
    type t = string Span.t
    val pretty_print: t -> Pp.branch
end = struct 
    type t = string Span.t
    let pretty_print n = Pp.(branch [text "INT"; spanned n] [])
end
and Str: sig
    type t = string Span.t
    val pretty_print: t -> Pp.branch
end = struct 
    type t = string Span.t
    let pretty_print n = Pp.(branch [text "STR"; spanned n] [])
end
and Foreign: sig
    type t = string Span.t
    val pretty_print: t -> Common.Pp.branch
end = struct 
    type t = string Span.t
    let pretty_print node = Pp.(branch [text "FOREIGN"; spanned node] [])
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
        params: Param.t list;
        block: Block.t
    }

    val pretty_print: t -> Pp.branch
end = struct 
    type t = {
        range: Span.range;
        params: Param.t list;
        block: Block.t
    }

    let pretty_print n = Pp.(branch [text "LAMBDA"] [
        branch [text "PARAMS"] (List.map n.params ~f: Param.pretty_print);
        (Block.pretty_print n.block)
    ])
end
and Let: sig
    type expr = 
        | Expr of Expr.t
        | Block of Block.t

    type t = {
        sig': Type.t option;
        range: Span.range;
        params: Param.t list option;
        ident: Ident.t;
        expr: expr;
        is_rec: bool
    }

    val expr_range: expr -> Span.range
    val pretty_print: t -> Pp.branch
end = struct 
    type expr = 
        | Expr of Expr.t
        | Block of Block.t

    type t = {
        sig': Type.t option;
        range: Span.range;
        params: Param.t list option;
        ident: Ident.t;
        expr: expr;
        is_rec: bool
    }

    let expr_range = function
    | Expr e -> Expr.range e
    | Block b -> b.range

    let pretty_print n = Pp.(branch [text "LET"; spanned n.ident] [])
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


    val pretty_print_stmt: block_stmt -> Pp.branch
    val pretty_print: t -> Pp.branch
    val make: ?range: Span.range -> block_stmt list -> t
    val expr: Expr.t -> block_stmt

end = struct 
    type block_stmt = 
        | Expr of Expr.t
        | Let of Let.t
        | Block of Block.t

    let pretty_print_stmt = function
        | Expr t -> Expr.pretty_print t
        | Let t -> Let.pretty_print t
        | Block t -> Block.pretty_print t

    type t = {
        range: Span.range;
        stmts: block_stmt list
    }

    let expr e = Block.Expr e

    let make ?range stmts = {
        stmts;
        range = match range with 
        | Some m -> m 
        | None -> Span.empty_range
    }

    let pretty_print n = Pp.(branch [text "BLOCK"] (n.stmts |> List.map ~f: pretty_print_stmt))
end
and Cond: sig
    type expr = 
        | Expr of Expr.t 
        | Block of Block.t
    type t = {range: Span.range; if_: expr; then_: expr; else_: expr option}

    val expr_range: expr -> Span.range 
    val pretty_print: t -> Pp.branch
end = struct 
    type expr = 
        | Expr of Expr.t 
        | Block of Block.t
    type t = {range: Span.range; if_: expr; then_: expr; else_: expr option}

    let expr_range = function
    | Expr e -> Expr.range e
    | Block b -> b.range

    let pretty_print _ = Pp.(branch [text "Hello"] [])
end
and Apply: sig
    type arg = 
        | PosArg of {expr: Expr.t}
        | NameArg of {name: string Span.t; expr: Expr.t}
        | PunArg of {name: string Span.t}

    type t = { fn: Expr.t; args: arg list; range: Span.range }

    val arg_range: arg -> Span.range
    val pretty_print: t -> Pp.branch
end = struct 
    type arg = 
        | PosArg of {expr: Expr.t}
        | NameArg of {name: string Span.t; expr: Expr.t}
        | PunArg of {name: string Span.t}

    type t = { fn: Expr.t; args: arg list; range: Span.range }

    let arg_range= function
        | PosArg {expr} -> Expr.range expr
        | NameArg {name; expr} -> Span.merge (name.range) (Expr.range expr)
        | PunArg {name} -> name.range

    let pretty_print app = 

        let open Pp in
        let pp_arg = function
            | PosArg {expr} -> branch [text "POS_ARG"] [Expr.pretty_print expr]
            | NameArg {name; expr} -> branch [text "NAME_ARG"; spanned name] [Expr.pretty_print expr]
            | PunArg {name} -> branch [text "PUN_ARG"; spanned name] []
        in
        branch [text "APPLY"] ([
            branch [text "FN"] [Expr.pretty_print app.fn];
        ] @ (List.map ~f:pp_arg app.args))
end
and Li: sig 
    type t = { range: Span.range; items: Expr.t list }

    val pretty_print: t -> Pp.branch
end = struct 
    type t = { range: Span.range; items: Expr.t list }

    let pretty_print node = 
        let items = node.items in
        Pp.(branch [text "LIST"] (List.map items ~f: Expr.pretty_print))
end
and Tuple: sig 
    type t = { range: Span.range; exprs: Expr.t list }

    val pretty_print: t -> Pp.branch
end = struct 
    type t = { range: Span.range; exprs: Expr.t list }

    let pretty_print node = 
        let exprs = node.exprs in
        Pp.(branch [text "TUPLE"] (List.map exprs ~f: Expr.pretty_print))
end
and Value: sig
    type t = 
        | Int of Int.t 
        | Str of Str.t 
        | Foreign of Foreign.t
        | Ident of Ident.t 
        | Lambda of Lambda.t 
        | Tuple of Tuple.t
        | Unit of Unit.t
        | Li of Li.t

    val range: t -> Span.range
    val pretty_print: t -> Pp.branch
end = struct 
    type t = 
        | Int of Int.t
        | Str of Str.t
        | Foreign of Foreign.t
        | Ident of Ident.t
        | Lambda of Lambda.t
        | Tuple of Tuple.t
        | Unit of Unit.t
        | Li of Li.t

    let pretty_print = function
        | Int v -> Int.pretty_print v
        | Str v -> Str.pretty_print v
        | Foreign v -> Foreign.pretty_print v
        | Ident v -> Ident.pretty_print v
        | Lambda v -> Lambda.pretty_print v 
        | Tuple v -> Tuple.pretty_print v
        | Unit v -> Unit.pretty_print v
        | Li v -> Li.pretty_print v
    
    let range = function
    | Int i -> i.range
    | Str s -> s.range
    | Ident i -> i.range
    | Lambda l -> l.range
    | Unit u -> u.range
    | Tuple t -> t.range
    | Foreign f -> f.range
    | Li l -> l.range
end
and Expr: sig
    type t = 
        | Value of Value.t
        | Apply of Apply.t
        | Cond of Cond.t
        | Match of Match.matc

    val range: Expr.t -> Span.range
    val pretty_print: t -> Pp.branch
end = struct 
    type t =
        | Value of Value.t
        | Apply of Apply.t
        | Cond of Cond.t
        | Match of Match.matc

    let pretty_print = function
        | Value v -> Value.pretty_print v
        | Apply a -> Apply.pretty_print a
        | Cond c -> Cond.pretty_print c
        | Match p -> Match.pretty_print_matc p

    let range = function
    | Value n -> Value.range n
    | Apply n -> n.range
    | Cond n -> n.range
    | Match n -> n.range
end
and Module: sig
    type entry = Let of Let.t | Import of Import.t | Module of Module.t

    type t = {
        range: Span.range;
        keyword: unit Span.t;
        name: string Span.t;
        entries: entry list
    }

    val pretty_print: t -> Pp.branch
end = struct 
    type entry = Let of Let.t | Import of Import.t | Module of Module.t
    type t = {
        range: Span.range;
        keyword: unit Span.t;
        name: string Span.t;
        entries: entry list
    }

    let pretty_print n = 
        Pp.(branch [text "MODULE"; spanned n.name] (
            List.map n.entries ~f: (function
                | Let t -> Let.pretty_print t
                | Import i -> Import.pretty_print i
                | Module m -> Module.pretty_print m
            )
        ))

end
and Match: sig 
    type pattern = 
        Param of string Span.t
        | Int of string Span.t
        | Str of string Span.t
        | Tuple of pattern list
        | List of {
            items: pattern list;
            rest: pattern option
        }

    val pretty_print_pattern: pattern -> Pp.branch 

    type case = {
        pattern: pattern;
        stmts: Block.block_stmt list
    }

    val pretty_print_case: case -> Pp.branch 

    type cases = {
        range: Span.range;
        cases: case list
    }

    val pretty_print_cases: cases -> Pp.branch 

    type matc = {
        range: Span.range;
        expr: Expr.t;
        block: cases;
    }

    val pretty_print_matc: matc -> Pp.branch
end = struct 
    type pattern = 
        Param of string Span.t
        | Int of string Span.t
        | Str of string Span.t
        | Tuple of pattern list
        | List of {
            items: pattern list;
            rest: pattern option
        }

    let rec pretty_print_pattern = function 
        | Param p -> Pp.(branch [text "PARAM"; text p.value] [])
        | Int p -> Pp.(branch [text "INT"; text p.value] [])
        | Str p -> Pp.(branch [text "STR"; text p.value] [])
        | Tuple t -> Pp.(branch [text "TUPLE"] (t |> List.map ~f:pretty_print_pattern))
        | List t -> 
            let items = t.items 
                |> List.map ~f:pretty_print_pattern
            in
            let rest = match t.rest with
                | Some m -> [Pp.(branch [text ".."] [pretty_print_pattern m])]
                | None -> []
            in
            Pp.(branch [text "LIST"] (items @ rest))

    type case = {
        pattern: pattern;
        stmts: Block.block_stmt list
    }

    let pretty_print_case case = 
        let sc = pretty_print_pattern case.pattern in
        let bl = List.map ~f:Block.pretty_print_stmt case.stmts in
        Pp.(branch [text "CASE"] [
            sc;
            Pp.(branch [text "BLOCK"] bl)
        ])

    type cases = {
        range: Span.range;
        cases: case list
    }

    let pretty_print_cases n = 
        Pp.(branch [text "CASES"] (List.map ~f:pretty_print_case n.cases))

    type matc = {
        range: Span.range;
        expr: Expr.t;
        block: cases;
    }

    let pretty_print_matc m = 
        Pp.(branch [text "MATCH"] [
            branch [text "EXPR"] [Expr.pretty_print m.expr];
            pretty_print_cases m.block
        ])
end

module Root = struct 
    type t = {
        entries: Module.entry list
    }
end