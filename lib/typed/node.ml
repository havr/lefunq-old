open Base
open Common

module rec Cond: sig 
    type case = {
        if_: Block.t;
        then_: Block.t
    }
    and t = {
        range: Span.range;
        cases: case list;
        else_: Block.t option;
    }

    val pretty_print: t -> Pp.branch
end = struct 
    type case = {
        if_: Block.t;
        then_: Block.t
    }
    and t = {
        range: Span.range;
        cases: case list;
        else_: Block.t option;
    }

    let pretty_print cond = 
        let cases = List.map cond.cases ~f: (fun case ->
            let if_branch = Pp.(branch [text "IF"] (List.map ~f: Stmt.pretty_print case.if_.stmts)) in 
            let then_branch = Pp.(branch [text "THEN"] (List.map ~f: Stmt.pretty_print case.then_.stmts)) in 
            Pp.(branch [text "CASE"] [if_branch; then_branch])
        ) in 
        let else_ = match cond.else_ with 
        | None -> []
        | Some else_ -> 
            [Pp.(branch [text "ELSE"] (List.map ~f:Stmt.pretty_print else_.stmts))]
        in
        Pp.(branch [text "COND"] (cases @ else_))
end
and Apply: sig
    type t = {
        range: Span.range;
        fn: Expr.t;
        args: Expr.t list;
    }

    val pretty_print: t -> Pp.branch
end = struct 
    type t = {
        range: Span.range;
        fn: Expr.t;
        args: Expr.t list; 
    }

    let pretty_print n = 
        let fn = [Expr.pretty_print n.fn] in
        let args = List.map n.args ~f: Expr.pretty_print in
        Pp.(branch [text "APPLY"]  (fn @ args))
end
and Value: sig
    type t = {
        value: string;
        type_: Type.t;
        range: Span.range
    }

    val to_string: t -> string
    val equals: t -> t -> bool
    val pretty_print: t -> Pp.branch
end = struct 
    type t = {
        value: string;
        type_: Type.t;
        range: Span.range
    }

    let pretty_print n = Pp.(branch [text "VALUE"; n.type_ |> Type.to_string |> text; text n.value] [])

    let to_string v = "value " ^ v.value ^ " :" ^ (Type.to_string v.type_)
    let equals a b = 
        let values_equal = phys_equal a.value b.value in
        let types_equal = Type.equals a.type_ b.type_ in
        values_equal && types_equal

end
and Foreign: sig 
    type t = {
        range: Span.range;
        name: string;
        scheme: Type.scheme option;
    }

    val pretty_print: t -> Pp.branch 
end = struct 
    type t = {
        range: Span.range;
        name: string;
        scheme: Type.scheme option;
    }

    let pretty_print m = 
        let scheme_str = match m.scheme with
        | None -> "<no scheme>"
        | Some m -> Type.scheme_to_string m
        in
        Pp.(branch [text "FOREIGN"; text m.name; text scheme_str] [])
end
and Ident: sig 
    type t = {
        range: Span.range;
        resolved: Symbol.Resolved.t; 
        resolution: Symbol.Resolved.t list;
        scheme: Type.scheme option;
    }

    val to_string: t -> string
    val equals: t -> t -> bool
    val pretty_print: t -> Pp.branch
end = struct 
    type t = {
        range: Span.range;
        resolved: Symbol.Resolved.t; 
        resolution: Symbol.Resolved.t list;
        scheme: Type.scheme option;
    }

    let pretty_print n = 
        let scheme = match n.scheme with
        | None -> "<no scheme>"
        | Some m -> Type.scheme_to_string m
        in
        let resolved r = 
            let abs = match Symbol.Resolved.(r.absolute) with
            | None -> "<unresolved>"
            | Some m -> Symbol.Id.to_string m
            in r.given ^ "(" ^ abs ^ ")"
        in
        let names = n.resolution 
            |> List.map ~f:resolved 
            |> String.concat ~sep: "."
        in Pp.(branch [
            text "IDENT"; 
            text @@ "resolved:" ^ (resolved n.resolved);
            text names;
            text @@ "scheme:" ^ scheme
        ] [])

    (* Use "equal" everywhere *)
    let equals a b =
        Symbol.Resolved.equal a.resolved b.resolved
        && (Symbol.Resolved.equal_path a.resolution b.resolution)
        && (Caml.(=) a.scheme b.scheme)

    let to_string id = 
        let scheme = match id.scheme with
        | None -> "unknown"
        | Some s -> Type.scheme_to_string s
        in

        String.concat [
            "IDENT";
            (Symbol.Resolved.to_string id.resolved); 
            "("; 
            (id.resolution |> List.map ~f:Symbol.Resolved.to_string |> String.concat ~sep: "."); 
            "): ";
            scheme
        ]

end
and Let: sig 
    type t = {
        range: Span.range;
        given_name: string;
        scope_name: string;
        scheme: Type.scheme option;
        is_rec: bool;
        block: Block.t;
        params: Param.t list;
        result: Type.t;
        sigt: Type.t option;
    }

    val pretty_print: t -> Pp.branch
end = struct 
    type t = {
        range: Span.range;
        given_name: string;
        scope_name: string;
        scheme: Type.scheme option;
        is_rec: bool;
        block: Block.t;
        params: Param.t list;
        result: Type.t;
        sigt: Type.t option;
    }
    let pretty_print n = 
        let sigt = Option.map n.sigt ~f: (fun sigt -> 
            Pp.(branch [text "SIGNATURE"; sigt |> Type.to_string |> text] [])
        ) in
        let scheme = Option.map n.scheme ~f: (fun sch -> 
            Pp.(branch [text "SCHEME"; sch |> Type.scheme_to_string |> text] [])
        ) in
        let result =  Pp.(branch [text "RESULT"; n.result |> Type.to_string |> text] []) in
        let params = match n.params with 
            | [] -> None
            | params -> Some (Pp.(branch [text "PARAMS"] (List.map params ~f: (Param.pretty_print))))
        in
        let block = [Pp.(branch [text "BLOCK"] (List.map n.block.stmts ~f: Stmt.pretty_print))] in
        let args = List.filter_map [sigt; scheme; Some result; params] ~f: (function | Some m -> Some m | None -> None) in
        Pp.(branch [text "LET"; text ("given:" ^ n.given_name); text ("scope:" ^ n.scope_name)] (args @ block))
end

and Stmt: sig 
    (* todo: let stmt and block stmt*)
    type t = Expr of Expr.t | Let of Let.t | Block of Block.t
    val equals: Stmt.t -> Stmt.t -> bool
    val to_string: Stmt.t -> string
    val range: Stmt.t -> Span.range
    val pretty_print: Stmt.t -> Pp.branch
end = struct 
    type t = Expr of Expr.t | Let of Let.t | Block of Block.t
    let range = function 
    | Expr e -> Expr.range e
    | Let let' -> let'.range
    | Block b -> b.range

    let equals a b = match (a, b) with
    | Expr _, Expr _ -> true (* TODO *)
    | Let _, Let _ -> true
    | Block ba, Block bb -> Block.equals ba bb
    | _, _ -> false

    let to_string = function
    | Expr e -> Expr.to_string e
    | Let b -> String.concat ["LET "; b.scope_name; "("; b.given_name; ")"; "\n"; Block.to_string b.block]
    | Block _ -> "block"

    let pretty_print = function 
        | Block b -> Block.pretty_print b
        | Let t -> Let.pretty_print t
        | Expr e -> Expr.pretty_print e
end
and Block: sig 
    type t = {range: Span.range; stmts: Stmt.t list}
    val equals: Block.t -> Block.t -> bool
    val to_string: Block.t -> string
    val last_stmt_range: Block.t -> Span.range
    val pretty_print: Block.t -> Pp.branch
end = struct 
    type t = {range: Span.range; stmts: Stmt.t list}

    let last_stmt_range bl = match bl.stmts with
        | [] -> bl.range
        | stmts -> Stmt.range (List.last_exn stmts)

    let pretty_print b = 
        let stmts = List.map b.stmts ~f: Stmt.pretty_print in 
            Pp.(branch [text "BLOCK"] stmts)

    let to_string block = List.map block.stmts ~f:Stmt.to_string 
        |> String.concat ~sep: "\n"
    let equals a b = if List.length a.stmts <> List.length b.stmts then false else
        List.zip_exn a.stmts b.stmts 
        |> List.map ~f:(fun (sa, sb) -> Stmt.equals sa sb) 
        |> List.find ~f:(fun entry -> not entry) 
        |> Option.is_none
end
and Tuple: sig 
    type t = {
        range: Span.range;
        exprs: Expr.t list
    }

    val pretty_print: t -> Pp.branch
end = struct
    type t = {
        range: Span.range;
        exprs: Expr.t list
    }

    let pretty_print node = Pp.(branch [text "TUPLE"] (List.map ~f: (Expr.pretty_print) node.exprs))
end
and Li: sig
    type t = {
        range: Span.range;
        items: Expr.t list
    }

    val pretty_print: t -> Pp.branch
end = struct 
    type t = {
        range: Span.range;
        items: Expr.t list
    }
     

    let pretty_print n = 
        let items = List.map n.items ~f: (Expr.pretty_print) in
        Pp.(branch [text "LIST"] items)
end
and Lambda: sig 
    type t = {
        range: Span.range;
        block: Block.t;
        params: Param.t list;
    }
    val pretty_print: t -> Pp.branch
end = struct 
    type t = {
        range: Span.range;
        block: Block.t;
        params: Param.t list;
    }

    let pretty_print n = 
        let params = List.map ~f:Param.pretty_print n.params in
        let params_branch = Pp.(branch [text "PARAMS"] params) in
        Pp.(branch [text "LAMBDA"] [params_branch; Block.pretty_print n.block])
end
and Match: sig 
    type pattern = 
        | Any
        | Unit
        | Int of string
        | Str of string
        | Param of {
            given_name: string;
            scope_name: string;
            typ: Type.t
        }
        | Tuple of pattern list
        | List of {
            item_typ: Type.t;
            items: pattern list;
            rest: pattern option
        }

    val pattern_to_type: pattern -> Type.t

    type case = {
        (* case_typ: Type.t; *)
        pattern: pattern;
        stmts: Stmt.t list
    }
    
    type t = {
        range: Span.range;
        typ: Type.t;
        expr: Expr.t;
        cases: case list
    }

    val pretty_print: t -> Pp.branch
end = struct 
    type pattern = 
        | Any
        | Unit
        | Int of string
        | Str of string
        | Param of {
            given_name: string;
            scope_name: string;
            typ: Type.t
        }
        | Tuple of pattern list
        | List of {
            item_typ: Type.t;
            items: pattern list;
            rest: pattern option
        }

    let rec pattern_to_type = function
        | Any -> Type.Unknown
        | Unit -> Base_types.unit
        | Int _ -> Base_types.int
        | Str _ -> Base_types.str
        | Tuple tup -> Type.Tuple (List.map ~f: pattern_to_type tup)
        | List li -> Base_types.list li.item_typ
        | Param p -> p.typ

    let rec pretty_print_pattern = function
        | Any -> Pp.(branch [text "_"] [])
        | Unit -> Pp.(branch [text "()"] [])
        | Int i -> Pp.(branch [text "INT"; text i] [])
        | Str s -> Pp.(branch [text "STR"; text s] [])
        | Param p -> Pp.(branch [text "PARAM"; text p.given_name; text p.scope_name; text @@ Type.to_string p.typ] [])
        | Tuple tup -> Pp.(branch [text "TUPLE"] (List.map tup ~f: pretty_print_pattern))
        | List li -> Pp.(branch [text "LIST"] [
            branch [text "ITEMS"] (List.map li.items ~f: pretty_print_pattern);
            branch [text ".."] (match li.rest with 
                | Some p -> [pretty_print_pattern p] 
                | None -> []
            )])

    type case = {
        (* case_typ: Type.t; *)
        pattern: pattern;
        stmts: Stmt.t list
    }

    let pretty_print_case n =
        Pp.(branch [text "CASE"(*; text @@ Type.to_string n.case_typ*)] [
            branch [text "PATTERN"] [pretty_print_pattern n.pattern];
            branch [text "SMTS"] (List.map ~f: Stmt.pretty_print n.stmts)
        ])

    type t = {
        range: Span.range;
        typ: Type.t;
        expr: Expr.t;
        cases: case list
    }

    let pretty_print n =
        Pp.(branch [text "MATCH"; text @@ Type.to_string n.typ] [
            branch [text "EXPR"] [Expr.pretty_print n.expr];
            branch [text "CASES"] (List.map ~f:pretty_print_case n.cases)
        ])
end
and Expr: sig 
    type t = 
        | Value of Value.t 
        | Ident of Ident.t
        | Apply of Apply.t
        | Lambda of Lambda.t
        | Cond of Cond.t
        | Li of Li.t
        | Tuple of Tuple.t
        | Foreign of Foreign.t
        | Match of Match.t

    val to_string: t -> string
    val range: t -> Span.range
    val pretty_print:  t -> Pp.branch
end = struct 
    type t = 
        | Value of Value.t
        | Ident of Ident.t
        | Apply of Apply.t
        | Lambda of Lambda.t
        | Cond of Cond.t
        | Li of Li.t
        | Tuple of Tuple.t
        | Foreign of Foreign.t
        | Match of Match.t

    let range = function
    | Value v -> v.range
    | Ident id -> id.range
    | Apply app -> app.range
    | Lambda lam -> lam.range
    | Cond cond -> cond.range
    | Tuple tup -> tup.range
    | Foreign f -> f.range
    | Li l -> l.range
    | Match m -> m.range

    let pretty_print = function
    | Value v -> Value.pretty_print v
    | Ident id -> Ident.pretty_print id
    | Apply app -> Apply.pretty_print app
    | Lambda lam -> Lambda.pretty_print lam
    | Cond cond -> Cond.pretty_print cond
    | Tuple tup -> Tuple.pretty_print tup
    | Foreign f -> Foreign.pretty_print f
    | Li l -> Li.pretty_print l
    | Match m -> Match.pretty_print m

    let to_string = function
    | Value v -> Value.to_string v
    | Ident id -> Ident.to_string id
    | Apply _ -> raise Common.TODO 
    | Lambda _ -> raise Common.TODO
    | Cond _ -> raise Common.TODO
    | Tuple _ -> raise Common.TODO
    | Foreign _ -> raise Common.TODO
    | Li _ -> raise Common.TODO
    | Match _ -> raise Common.TODO
end

module Import = struct 
    type name = {
        name: string Span.t;
        path: string Span.t list;
        resolved: Symbol.Module.exposed option;
    }

    type t = {
        resolved_source: string;
        source: string Span.t;
        names: name list;
    }

    let pretty_print n = 
        let open Pp in
        let exact = List.map n.names ~f: (fun m -> 
            let resolved = match m.resolved with
                | None -> []
                | Some {typedef; modu; binding} -> 
                    Option.map typedef ~f:(fun _ -> branch [text "typedef"] [])
                    :: Option.map modu ~f:(fun m -> branch [text "module"; text (Symbol.Id.to_string m.id)] [])
                    :: Option.map binding ~f:(fun m -> branch [
                        text "binding";
                        text (Symbol.Id.to_string m.id);
                    ] [])
                    :: []
                    |> List.filter ~f: (Option.is_some)
                    |> List.map ~f: (fun op -> Option.value_exn op)
            in
            branch [spanned m.name; m.path |> List.map ~f: (fun s -> Span.(s.value)) |> String.concat |> text] resolved
        ) in
        branch [text "import"; spanned n.source] exact
end

module Module = struct
    type entry = Binding of Let.t | Import of Import.t | Module of t
    and t = {
        given_name: string;
        scope_name: string;
        entries: entry list;
        exposed: Symbol.Module.exposed StringMap.t;
    }

    let rec pretty_print n = 
        let entries = List.map n.entries ~f: (function 
            | Binding t -> Let.pretty_print t
            | Import i -> Import.pretty_print i
            | Module m -> pretty_print m
        ) in
        Pp.(branch [text "MODULE"; text n.given_name; text n.scope_name] entries)
end