open Common
open Base


module Type = struct 
    type t = Simple of {
        name: string Span.t;
        args: t list;
    }
    | Var of {
        name: string Span.t;
    }
    | List of t
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
        | Var v -> Pp.(branch [text "Var"; spanned v.name] [])
        | Lambda la -> Pp.(branch [text "LAMBDA"] [pretty_print la.arg; pretty_print la.result])
        | Tuple t -> Pp.(branch [text "TUPLE"] (List.map t.items ~f: pretty_print))
        | List li -> Pp.(branch [text "LIST"] [pretty_print li])
        | Unit -> Pp.(branch [text "()"] [])
end

module Using = struct 
    type name = 
        | Ident of {
            name: string Span.t;
            action: action option;
        }
        | Wildcard of string Span.t

    and action = 
        | Rename of string Span.t
        | Nested of (name list)
    
    type kind =
        | Local of string Span.t
        | Global of string Span.t
    type t = {
        keyword: string Span.t;
        kind: kind;
        action: action;
    }

    let pretty_print n = 
        let open Common.Pp in
        let kind = match n.kind with
            | Local loc -> spanned loc
            | Global glob -> spanned glob
        in
        let rec action a = match a with
            | Rename n -> [branch [text "AS"; spanned n] []]
            | Nested names -> List.map names ~f: (function 
                | Wildcard w -> branch [spanned w] []
                | Ident id -> 
                    let act = match id.action with
                        | Some a -> action a
                        | None -> []
                    in
                    branch [spanned id.name] act)
        in
        branch [spanned n.keyword; kind] (action n.action)
end
module Unit = struct
    type t = unit Span.t
    let pretty_print _ = Pp.(branch [text "()"] [])
end


module Typedef = struct 
    type def = 
        | Foreign of Span.range

    type params = {var: string Span.t}

    type t = {
        name: string Span.t;
        params: params list;
        def: def
    }

    let pretty_print n = 
        let def = match n.def with
            | Foreign _ -> Pp.(branch [text "FOREIGN"] [])
        in
        let params = n.params |> List.map ~f:(fun {var} -> Pp.spanned var) in
        Pp.(branch ([text "TYPE"; spanned n.name] @ params) [def])
end 

module Destruct = struct 
    type shape = 
        | Unit
        | Name of string Span.t
        | Tuple of shape list

    open Pp

    let rec pretty_print = function
        | Unit -> branch [text "()"] []
        | Name n -> branch [spanned n] []
        | Tuple t -> branch [text "tuple"] (List.map t ~f: pretty_print)
end


module rec Dummy: sig end = struct end


and FuncParam: sig 
    type typed = {
        name: Ident.t;
        typ: Type.t;
    }

    type shape = | Unit | Name of typed | Tuple of shape list

    type named_kind = | Typed of Type.t | Shaped of shape

    type optinonal_kind = | WithDefault of Expr.t | Optional of Type.t

    type t = 
        | Positional of shape
        | Named of {
            name: Ident.t;
            kind: named_kind;
        }
        | Optional of {
            name: Ident.t;
            kind: optinonal_kind;
        }
        | Extension of typed

    val pp: t -> Pp.branch
end = struct 
    type typed = {
        name: Ident.t;
        typ: Type.t;
    }

    type shape = | Unit | Name of typed | Tuple of shape list

    type named_kind = | Typed of Type.t | Shaped of shape

    type optinonal_kind = | WithDefault of Expr.t | Optional of Type.t

    type t = 
        | Positional of shape
        | Named of {
            name: Ident.t;
            kind: named_kind;
        }
        | Optional of {
            name: Ident.t;
            kind: optinonal_kind;
        }
        | Extension of typed


    let pp = 
    (* let open Common.Pp in  *)
    (* let open Node.FnParam in  *)
    let rec print_shape = Pp.(function
        | Unit -> branch [text "()"] []
        | Name n -> branch [spanned n.name] [Type.pretty_print n.typ]
        | Tuple shs -> branch [text "tuple"] (List.map ~f: print_shape shs)
    ) in
    Pp.(function
            | Positional sh -> print_shape sh
            | Named p -> 
                let value = (match p.kind with
                  | Typed s -> [Type.pretty_print s]
                  | Shaped sh -> [print_shape sh]
                ) in 
                branch [text "&"; spanned p.name] value
            | Optional p ->
                let value = (match p.kind with
                  | WithDefault e -> [Expr.pretty_print e]
                  | Optional typ -> [Type.pretty_print typ]
                ) in 
                branch [text "?"; spanned p.name] value
            | Extension p ->
                Pp.(branch [text "..."; spanned p.name] [
                    Type.pretty_print p.typ
                ])
    )

end

and Param: sig 
    type t = 
        | Positional of {
            shape: Destruct.shape;
        }
        | Named of {
            name: string Span.t;
            shape: Destruct.shape option;
        }
        | Optional of {
            name: string Span.t;
            alias: string Span.t option;
            default: Expr.t option
        }
        | Extension of {
            name: string Span.t;
            type_ident: Type.t;
        }

    val pretty_print: t -> Pp.branch
end = struct
    type t = 
        | Positional of {
            shape: Destruct.shape;
        }
        | Named of {
            name: string Span.t;
            shape: Destruct.shape option;
        }
        | Optional of {
            name: string Span.t;
            alias: string Span.t option;
            default: Expr.t option
        }
        | Extension of {
            name: string Span.t;
            type_ident: Type.t;
        }

    let pretty_print = function
        | Positional p -> 
            Destruct.pretty_print p.shape
        | Named p -> 
            let shape = match p.shape with
                | Some s -> [Destruct.pretty_print s]
                | None -> []
            in
            Pp.(branch [text "&"; spanned p.name] shape)
        | Optional p ->
            let alias = match p.alias with
                | Some s -> Pp.[text "as"; spanned s]
                | None -> []
            in
            let default = match p.default with
                | None -> []
                | Some m -> [Expr.pretty_print m]
            in
            Pp.(branch ([text "?"; spanned p.name] @ alias) default)
        | Extension p ->
            Pp.(branch [text "&.."; spanned p.name] [Type.pretty_print p.type_ident])
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
    type t = {
        name: string Span.t;
        typ: Type.t
    }
    val pretty_print: t -> Common.Pp.branch
end = struct 
    type t = {
        name: string Span.t;
        typ: Type.t
    }
    let pretty_print node = Pp.(branch [text "FOREIGN"; spanned node.name] [Type.pretty_print node.typ])
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
and Func: sig 
    type expr = 
        | Expr of Expr.t
        | Block of Block.t
        | Foreign of unit

    type t = {
        range: Span.range;

        name: Ident.t;
        params: FuncParam.t list;
        return: Type.t option;
        expr: expr;
        is_rec: bool
    }

    val expr_range: expr -> Span.range
    val pp: t -> Pp.branch
end = struct 
    type expr = 
        | Expr of Expr.t
        | Block of Block.t
        | Foreign of unit

    type t = {
        range: Span.range;

        name: Ident.t;
        params: FuncParam.t list;
        return: Type.t option;
        expr: expr;
        is_rec: bool
    }
    let expr_range = function
    | Expr e -> Expr.range e
    | Block b -> b.range
    | Foreign () -> Span.empty_range

    let pp n = Pp.(
        let pp_expr = match n.expr with
            | Expr e -> Expr.pretty_print e
            | Block b -> Block.pretty_print b
            | Foreign _ -> branch [text "FOREIGN"] []
        in
        let head = if n.is_rec then [text "FUNC"; text "(rec)"] else [text "FUNC"]
        in
            branch (head @ [spanned n.name]) [
                branch [text "PARAMS"] (List.map ~f: FuncParam.pp n.params);
                pp_expr 
            ]
    )
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
        | Using of Using.t

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
        | Using of Using.t

    let pretty_print_stmt = function
        | Expr t -> Expr.pretty_print t
        | Let t -> Let.pretty_print t
        | Block t -> Block.pretty_print t
        | Using u -> Using.pretty_print u

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
    type item = 
        | Single of Expr.t
        | Spread of Expr.t

    type t = { range: Span.range; items: item list }

    val pretty_print: t -> Pp.branch
end = struct 
    type item = 
        | Single of Expr.t
        | Spread of Expr.t

    type t = { range: Span.range; items: item list }

    let pretty_print node = 
        let item = function
            | Single ex -> Expr.pretty_print ex
            | Spread ex -> Pp.(branch [text ".."] [Expr.pretty_print ex])
        in
        Pp.branch [Pp.text "LIST"] (List.map node.items ~f: item)
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
    | Foreign f -> f.name.range (* TODO: return range of the node *)
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
    type entry = 
        | Let of Let.t 
        | Func of Func.t 
        | Using of Using.t 
        | Module of Module.t 
        | Typedef of Typedef.t

    type t = {
        range: Span.range;
        keyword: unit Span.t;
        name: string Span.t;
        entries: entry list
    }

    val pretty_print: t -> Pp.branch
    val pp_entry: entry -> Pp.branch
end = struct 
    type entry = 
        | Let of Let.t 
        | Func of Func.t 
        | Using of Using.t 
        | Module of Module.t 
        | Typedef of Typedef.t

    type t = {
        range: Span.range;
        keyword: unit Span.t;
        name: string Span.t;
        entries: entry list
    }

    let pp_entry = function
        | Let t -> Let.pretty_print t
        | Func t -> Func.pp t
        | Using  i -> Using.pretty_print i
        | Module m -> Module.pretty_print m
        | Typedef t -> Typedef.pretty_print t
    let pretty_print n = 
        Pp.(branch [text "MODULE"; spanned n.name] (List.map n.entries ~f: pp_entry))

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
and Struct: sig 
    type elem = 
        | Pun of (string Span.t)
        | Value of (string Span.t * (Expr.t))
        | Group of elem list

    type t = {
        keyword: string Span.t;
        elems: elem list
    }
end = struct 
    type elem = 
        | Pun of (string Span.t)
        | Value of (string Span.t * (Expr.t))
        | Group of elem list

    type t = {
        keyword: string Span.t;
        elems: elem list
    }
end

module Root = struct 
    type t = {
        entries: Module.entry list
    }
end
