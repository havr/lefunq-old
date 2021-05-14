open Base
open Common
open Typed_common

(* TODO (refactor): 
    - move pp/stuff out
    - add typed / spanned annotations everywhere
*)
module Using = struct 
    type import = Wildcard | Only of (string Span.t)
    type root = Source of {
        name: string Span.t;
        resolved: string
    } | Local of string Span.t

    type t = {
        range: Span.range;
        root: root;
        names: (import * string Span.t list) list;
        resolved: (string * Resolved.Module.exposed) list;
    }
end
module TypeIdent = struct 
    type arg = {label: string; optional: bool; typ: t}
    and t = Simple of {
        given: string Span.t;
        name: Type.name;
        args: t list;
    }
    | Var of {
        given: string Span.t
    }
    | Lambda of {
        arg: arg;
        result: t;
    } 
    | Tuple of { items: t list; }
    | List of t
    | Unit
end

module Typedef = struct 
    type param = {var: string Span.t}
    type t = {
        name: string Span.t;
        scope_name: string;
        params: param list;
        def: Type.def;
    }
end

module Destruct = struct 
    type name = {given: string; scope: string; typ: Type.t}

    type shape = 
        | Unit
        | Name of name
        | Tuple of shape list

end

module rec Cond: sig 
    type case = {
        if_: Block.t;
        then_: Block.t
    }
    type t = {
        typ: Type.t;
        range: Span.range;
        cases: case list;
        else_: Block.t option;
    }
end = struct 
    type case = {
        if_: Block.t;
        then_: Block.t
    }

    type t = {
        typ: Type.t;
        range: Span.range;
        cases: case list;
        else_: Block.t option;
    }
end
and Param: sig 
    type value =
        | Positional of {shape: Destruct.shape}
        | Named of {given: string Span.t; shape: Destruct.shape}
        | Optional of {given: string Span.t; scope: string; alias: string Span.t option; default: Expr.t option}
        | Extension of {given: string Span.t}

    type t = {
        typ: Type.t;
        value: value 
    }

    (* val names: t -> name list *)
    val equals: t -> t -> bool
    (* val name: string -> string -> name *)
end = struct 
    type value =
        | Positional of {shape: Destruct.shape}
        | Named of {given: string Span.t; shape: Destruct.shape}
        | Optional of {given: string Span.t; scope: string; alias: string Span.t option; default: Expr.t option}
        | Extension of {given: string Span.t}

    type t = {
        typ: Type.t;
        value: value 
    }

    let equals a b = phys_equal a b
end
and Apply: sig
    type arg = 
        | PosArg of {expr: Expr.t}
        | NameArg of {name: string Span.t; expr: Expr.t}

    type t = {
        typ: Type.t;
        range: Span.range;
        fn: Expr.t;
        args: arg list;
    }

    val arg_range: arg -> Span.range
end = struct 
    type arg = 
        | PosArg of {expr: Expr.t}
        | NameArg of {name: string Span.t; expr: Expr.t}

    type t = {
        typ: Type.t;
        range: Span.range;
        fn: Expr.t;
        args: arg list;
    }

    let arg_range= function
        | PosArg {expr} -> Expr.range expr
        | NameArg {name; expr} -> Span.merge (name.range) (Expr.range expr)

end
and Value: sig
    type t = {
        typ: Type.t;
        range: Span.range;
        value: string;
    }

end = struct 
    type t = {
        typ: Type.t;
        range: Span.range;
        value: string;
    }
end
and Foreign: sig 
    type t = {
        type_ident: TypeIdent.t;
        range: Span.range;
        name: string;
        typ: Type.t;
        scheme: Type.scheme;
    }
end = struct 
    type t = {
        type_ident: TypeIdent.t;
        range: Span.range;
        name: string;
        typ: Type.t;
        scheme: Type.scheme;
    }

end
and Ident: sig 
    type t = {
        typ: Type.t;
        scheme: Type.scheme option;
        range: Span.range;
        qual: Qualified.t;
        (* scheme: Type.scheme option; *)
    }

    (* val equals: t -> t -> bool *)
end = struct 
    type t = {
        typ: Type.t;
        scheme: Type.scheme option;
        range: Span.range;
        qual: Qualified.t;
    }
    (* Use "equal" everywhere *)
    (* let equals a b =
        Symbol.Resolved.equal a.resolved b.resolved
        && (Symbol.Resolved.equal_path a.resolution b.resolution)
        && (Caml.(=) a.scheme b.scheme) *)
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
        (* remove *)
        sigt: Type.t option;
    }
end

and Stmt: sig 
    (* todo: let stmt and block stmt*)
    type t = 
        Expr of Expr.t 
        | Let of Let.t 
        | Block of Block.t
        | Using of Using.t

    val range: t -> Span.range
end = struct 
    type t = 
        Expr of Expr.t 
        | Let of Let.t 
        | Block of Block.t
        | Using of Using.t

    let range = function
        | Expr e -> Expr.range e
        | Let l -> l.range
        | Block b -> b.range
        | Using u -> u.range
end
(* TODO: remove *)
and Block: sig 
    type t = {stmts: Stmt.t list; range: Span.range}
    (* val equals: Block.t -> Block.t -> bool *)
    val last_stmt_range: Block.t -> Span.range
end = struct 
    type t = {stmts: Stmt.t list; range: Span.range}

    let last_stmt_range b = Stmt.range (List.hd_exn b.stmts)
end
and Tuple: sig 
    type t = {
        typ: Type.t;
        range: Span.range;
        exprs: Expr.t list
    }
end = struct
    type t = {
        typ: Type.t;
        range: Span.range;
        exprs: Expr.t list
    }
end
and Li: sig
    type item = 
        | Single of Expr.t
        | Spread of Expr.t

    type t = {
        typ: Type.t;
        range: Span.range;
        items: item list
    }
end = struct 
    type item = 
        | Single of Expr.t
        | Spread of Expr.t
    type t = {
        typ: Type.t;
        range: Span.range;
        items: item list
    }
end
and Lambda: sig 
    type t = {
        typ: Type.t;
        range: Span.range;
        block: Block.t;
        params: Param.t list;
    }
end = struct 
    type t = {
        typ: Type.t;
        range: Span.range;
        block: Block.t;
        params: Param.t list;
    }
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

    type case = {
        (* case_typ: Type.t; *)
        pattern: pattern;
        stmts: Stmt.t list
    }
    
    type t = {
        typ: Type.t;
        range: Span.range;
        expr: Expr.t;
        cases: case list
    }
    val pattern_to_type: pattern -> Type.t
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

    type case = {
        (* case_typ: Type.t; *)
        pattern: pattern;
        stmts: Stmt.t list
    }

    type t = {
        typ: Type.t;
        range: Span.range;
        expr: Expr.t;
        cases: case list
    }
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

    val range: t -> Span.range
    val typ: t -> Type.t
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
        | Li li -> li.range
        | Tuple tu -> tu.range
        | Foreign fo -> fo.range
        | Match ma -> ma.range

    let typ = function
        | Value v -> v.typ
        | Ident id -> id.typ
        | Apply app -> app.typ
        | Lambda lam -> lam.typ
        | Cond cond -> cond.typ
        | Li li -> li.typ
        | Tuple tu -> tu.typ
        | Foreign fo -> fo.typ
        | Match ma -> ma.typ
end


module Module = struct
    type entry = 
        | Binding of Let.t 
        | Using of Using.t 
        | Module of t
        | Typedef of Typedef.t
    and t = {
        given_name: string;
        scope_name: string;
        entries: entry list;
        exposed: Resolved.Module.exposed StringMap.t;
    }
end

module Print_node = struct 
    open Pp 

    (* let typed ~fn n = 
        text ((Span.range_str n.range) ^ ":" ^ (Type.to_string n.typ) ^ "#" ^ () *)

    let rec destruct shape = 
        match shape with
        | Destruct.Unit -> branch [text "()"] []
        | Destruct.Name n -> branch [text n.given; text n.scope; text @@ Type.to_string n.typ] []
        | Destruct.Tuple t -> branch [text "tuple"] (List.map t ~f: destruct)

    let typedef n = 
        let def_str = match Typedef.(n.def) with
            | Type.Foreign -> branch [text "foreign"] []
            | Type.Unresolved -> branch [text "<unresolved>"] []
        in
        Pp.(branch [text "TYPE"; spanned n.name; text n.scope_name] [def_str])

    let rec type_ident = function
        | TypeIdent.Var v -> 
            Pp.(branch [text "VAR"; spanned v.given] [])
        | TypeIdent.Simple t -> 
            let qual_name = t.name.resolved.path @ [t.name.resolved.name]
            |> List.map ~f: (Resolved_name.to_string)
            |> String.concat ~sep: "/" in
            Pp.(branch [text "TYPE"; text qual_name] (List.map ~f: type_ident t.args))
        | TypeIdent.Lambda {arg; result} -> 
            let arg_node = if not @@ String.is_empty arg.label then (
                [Pp.branch [text  @@ arg.label ^ (if arg.optional then "?" else "") ^ ":"] []]
            ) else [] in
            Pp.(branch [text "LAMBDA"] (arg_node @ [type_ident arg.typ; type_ident result]))
        | TypeIdent.Tuple t -> Pp.(branch [text "TUPLE"] (List.map t.items ~f: type_ident))
        | TypeIdent.List li -> Pp.(branch [text "list"] [type_ident li])
        | TypeIdent.Unit -> Pp.(branch [text "()"] [])

    let rec modu n = 
        let entries = List.map Module.(n.entries) ~f: (function 
            | Typedef t -> typedef t
            | Binding t -> let' t
            | Using u -> using u
            | Module m -> modu m
        ) in
        Pp.(branch [text "MODULE"; text n.given_name; text n.scope_name] entries)
    and using n = 
        let resolved = (fun Resolved.Module.{typedef; modu; binding} ->
            Option.map typedef ~f:(fun _ -> branch [text "typedef"] [])
            :: Option.map modu ~f:(fun m -> branch [text "module"; text (Id.to_string m.id)] [])
            :: Option.map binding ~f:(fun m -> 
                let id = Resolved.Binding.(match m with
                    | Local loc -> loc 
                    | Global g -> (Id.to_string g.id) ^ " " ^ (Type.scheme_to_string g.scheme)) in
                branch [
                    text "binding";
                    text id;
                ] [])
            :: []
            |> List.filter ~f: (Option.is_some)
            |> List.map ~f: (fun op -> Option.value_exn op)
        )
        in
        let names = 
            let ns = List.map Using.(n.names) ~f: (fun (imp, path) ->
                let imp = match imp with
                    | Using.Only n -> spanned n
                    | Using.Wildcard -> text "*"
                in
                branch ([imp; text "from"] @ (List.map path ~f:spanned)) []
            ) in branch [text "names"] ns
        in
        let resolved = 
            let ws = List.map Using.(n.resolved) ~f: (fun (name, res) ->
                branch [text name] (resolved res)
            ) in branch [text "resolved"] ws
        in
        let root = match n.root with
            | Using.Local loc -> [spanned loc]
            | Using.Source {resolved; name} -> [spanned name; text resolved]
        in branch ([text "import"] @ root) [names; resolved]

    and expr e = 
        let open Expr in 
        match e with
        | Value v -> value v
        | Ident id -> ident id
        | Apply app -> apply app
        | Lambda lam -> lambda lam
        | Cond c -> cond c
        | Tuple tup -> tuple tup
        | Foreign f -> foreign f
        | Li l -> list l
        | Match m -> matc m

    and pattern p = 
        let open Match in 
        match p with
        | Any -> branch [text "_"] []
        | Unit -> branch [text "()"] []
        | Int i -> branch [text "INT"; text i] []
        | Str s -> branch [text "STR"; text s] []
        | Param p -> branch [text "PARAM"; text p.given_name; text p.scope_name; text @@ Type.to_string p.typ] []
        | Tuple tup -> branch [text "TUPLE"] (List.map tup ~f: pattern)
        | List li -> branch [text "LIST"] [
            branch [text "ITEMS"] (List.map li.items ~f: pattern);
            branch [text ".."] (match li.rest with 
                | Some p -> [pattern p] 
                | None -> []
            )]
    and case n =
        branch [text "CASE"(*; text @@ Type.to_string n.case_typ*)] [
            branch [text "PATTERN"] [pattern Match.(n.pattern)];
            branch [text "SMTS"] (List.map ~f:stmt n.stmts)
        ]

    and matc n =
        branch [text "MATCH"; text @@ Type.to_string n.typ] [
            branch [text "EXPR"] [expr n.expr];
            branch [text "CASES"] (List.map ~f:case n.cases)
        ]

    and lambda n = 
        let params = List.map ~f:param n.params in
        let params_branch = Pp.(branch [text "PARAMS"] params) in
        branch [text "LAMBDA"; text (Type.to_string n.typ)] [params_branch; block n.block]

    and list n = 
        let item = function
            | Li.Single ex -> expr ex
            | Li.Spread ex -> Pp.(branch [text ".."] [expr ex])
        in
        let items = List.map n.items ~f:item in
        branch [text "LIST"] items

    and tuple n = branch [text "TUPLE"] (List.map ~f:expr n.exprs)

    and block n = 
        let stmts = List.map n.stmts ~f: stmt in 
            branch [text "BLOCK"] stmts

    and stmt n = 
        let open Stmt in 
        match n with 
        | Block b -> block b
        | Let t -> let' t
        | Expr e -> expr e
        | Using u -> using u

    and let' n = 
        let sigt = Option.map n.sigt ~f: (fun sigt -> 
            branch [text "SIGNATURE"; sigt |> Type.to_string |> text] []
        ) in
        let scheme = Option.map n.scheme ~f: (fun sch -> 
            branch [text "SCHEME"; sch |> Type.scheme_to_string |> text] []
        ) in
        let result = branch [text "RESULT"; n.result |> Type.to_string |> text] [] in
        let params = match n.params with 
            | [] -> None
            | params -> Some (branch [text "PARAMS"] (List.map params ~f: param))
        in
        let block = [branch [text "BLOCK"] (List.map n.block.stmts ~f: stmt)] in
        let args = List.filter_map [sigt; scheme; Some result; params] ~f: (function | Some m -> Some m | None -> None) in
        branch [text "LET"; text ("given:" ^ n.given_name); text ("scope:" ^ n.scope_name)] (args @ block)

    and ident n = 
        let resolved r = 
            let abs = match Resolved_name.(r.resolved) with
            | None -> "<unresolved>"
            | Some m -> Id.to_string m
            in r.given ^ "(" ^ abs ^ ")"
        in
        let names = n.qual.path
            |> List.map ~f:resolved 
            |> String.concat ~sep: "."
        in branch [
            text "IDENT"; 
            text @@ "resolved:" ^ (resolved n.qual.name);
            text names;
            text @@ "type:" ^ (Type.to_string n.typ)
        ] []

    and foreign m = 
        branch [text "FOREIGN"; text m.name; text (Type.to_string m.typ)] [type_ident m.type_ident]

    and value n = branch [text "VALUE"; n.typ |> Type.to_string |> text; text n.value] []

    and apply n = 
        let open Apply in 
        let pp_arg = function
            | PosArg {expr = e} -> branch [text "POS_ARG"] [expr e]
            | NameArg {name; expr = e} -> branch [text "NAME_ARG"; spanned name] [expr e]
            (* | PunArg {ident} -> branch [text "PUN_ARG"] [Ident.pretty_print ident] *)
        in
        let fn = [expr n.fn] in
        let args = List.map n.args ~f: pp_arg in
        (* TODO: debug result *)
        branch [text "APPLY"; text @@ Type.to_string n.typ]  (fn @ args)

    and cond c = 
        let cases = List.map c.cases ~f: (fun case ->
            let if_branch = Pp.(branch [text "IF"] (List.map ~f: stmt case.if_.stmts)) in 
            let then_branch = Pp.(branch [text "THEN"] (List.map ~f: stmt case.then_.stmts)) in 
            Pp.(branch [text "CASE"] [if_branch; then_branch])
        ) in 
        let else_ = match c.else_ with 
        | None -> []
        | Some else_ -> 
            [branch [text "ELSE"] (List.map ~f:stmt else_.stmts)]
        in
        branch [text "COND"] (cases @ else_)

    and param p = 
        let typ = text @@ Type.to_string @@ Param.(p.typ) in
        match Param.(p.value) with
        | Param.Positional {shape} -> branch [text "positional"; typ] [destruct shape]
        | Param.Named {given; shape} -> branch [text "name"; spanned given; typ] [destruct shape]
        | Param.Optional {given; scope; alias; default} -> 
            let alias = match alias with
            | None -> []
            | Some a -> Pp.[text "as"; spanned a]
            in
            let default = match default with
            | None -> []
            | Some e -> [expr e]
            in branch ([text "optional"; spanned given; text scope; typ] @ alias) default
        | Param.Extension {given} -> 
            branch [text "ext"; spanned given; typ] []
end