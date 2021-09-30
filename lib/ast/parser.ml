open Base
open Common

open Comb
open Comb.Syntax

(* TODO: move somewhare *)
let forward_decl (type a) (type r) () = 
    let fn: (a -> r) Option.t ref = ref None in
    let setter value = 
        match !fn with
        | None -> fn := (Some value)
        | Some _ -> raise (Invalid_argument "the value is initialized")
    in let getter arg = 
        match !fn with
        | None -> raise (Invalid_argument "the forward value is not initialized")
        | Some fn -> fn arg
    in getter, setter


exception Todo of string

let make_state = State.make

let match_map = one_value
let match_replace = one

module Lexemes = struct 
    open Lexeme

    let import = match_map (function Import -> Some "import" | _ -> None)
    let ident = match_map (function Ident n -> Some n | _ -> None)
    let ident_of opts = match_map (function
        | Ident n -> List.find opts ~f: (String.equal n)
        | _ -> None
    )

    let str = match_map (function Str s -> Some s | _ -> None)
    let operator = match_map (function | Op op -> Some op | _ -> None)
    let operator_of values = match_map (function 
        | Op op -> List.find values ~f:(String.equal op) 
        | _ -> None
    )
    let let' = match_map (function Let -> Some () | _ -> None)
    let rec_ = match_map (function | Rec -> Some() | _ -> None) 
    let int = match_map (function Int int -> Some int | _ -> None) 
    let if' = match_map(function If -> Some() | _ -> None) 
    let then' = match_map(function Then -> Some() | _ -> None)  
    let else' = match_map(function Else -> Some() | _ -> None)  
    let eq = match_map(function Eq -> Some () | _ -> None)
    let open_block = match_map(function | OpenBlock -> Some() | _ -> None)
    let close_block = match_map(function | CloseBlock -> Some() | _ -> None)
    let semi = match_map(function | Semi -> Some() | _ -> None)
    let stmt_separator = match_map(function | LineBreak -> Some() | Semi -> Some() | _ -> None)
    let lambda = match_map(function | Lambda -> Some () | _ -> None)
    let open_bracket = match_map(function | OpenBracket -> Some() | _ -> None) 
    let close_bracket = match_map(function | CloseBracket -> Some() | _ -> None)
    let open_paren = match_map(function | OpenParen -> Some() | _ -> None) 
    let close_paren = match_map(function | CloseParen -> Some() | _ -> None)
    let line_break = match_map(function | LineBreak -> Some() | _ -> None)
    let coma = match_map(function | Coma -> Some () | _ -> None)
    let sig' = match_map(function | Sig -> Some () | _ -> None)
    let func_arrow = match_map(function | FuncArrow -> Some () | _ -> None)
    let foreign = match_map(function | Foreign -> Some () | _ -> None)
    let modu = match_map(function | Module -> Some () | _ -> None)
    let pipe = match_map(function | Pipe -> Some () | _ -> None)
    let spread = match_map(function | Spread -> Some () | _ -> None)
    (* TODO: s/matc/question *)
    let matc = match_map(function | Match -> Some () | _ -> None)
    let ampersand = match_map(function | Ampersand -> Some () | _ -> None)
    let colon = match_map(function | Colon -> Some () | _ -> None)
    let type' = match_map(function | Type -> Some () | _ -> None)

    let struct' = match_map(function | Struct -> Some () | _ -> None)
end

let throw e = {fn = fun _ -> Error e}

let precedence = [ ["|>"]; ["$"]; ["+"; "-"; "++"]; ["*"; "/"; "%"]; [">"; "<"; "=="; "!="]]

let wrap p = {fn = p.fn}

let newlines = let+ _ = one_more @@ Lexemes.line_break in ()

let ignore_newline p = 
    let+ _ = many @@ Lexemes.line_break 
    and+ v = p in v

(* TODO: move it in parlex or common *)
let forward_fun () = 
    let value = ref None in
    let proxy arg = match !value with 
        | None -> raise (Invalid_argument ("forward declaration hasn't been initialized yet"))
        | Some fn -> fn arg
    in
    let init_value v = match !value with
        | None -> value := (Some v)
        | Some _ -> raise (Invalid_argument ("forward declaration is initialized twice"))
    in proxy, init_value

let (modu: unit -> Node.Module.t Comb.t) , init_modu  = forward_fun ()

let expect ?ctx ?exp p =  { fn = fun state ->
    match p.fn state with
    | Ok k -> Ok k
    | Error e -> 
        if e.no_match then 
            let lex_str = e.caused_by |> Lexeme.to_string in
            let unexp_str = "unexpected \"" ^ lex_str ^ "\"" in
            let ctx_str = match ctx with | Some ctx -> " while " ^ ctx | None -> "" in
            let exp_str = match exp with | Some exp -> ". expecting " ^ exp | None -> "" in
            let total = [unexp_str; ctx_str; exp_str] 
                |> List.filter ~f: (fun str -> not @@ String.is_empty str) 
                |> String.concat ~sep:"" in
            Error {e with no_match = false; err_msg = total}
        else Error e
}

(* TODO: move to some kind of "common" *)
let is_ident_uppercase name = 
    let rec trim_underscores = function
        | "" -> ""
        | str -> if Char.equal (String.get str 0) '_' 
            then String.sub str ~pos:0 ~len:1 |> trim_underscores 
            else str
    in Char.is_uppercase @@ String.get (trim_underscores name) 0

let choicef parserfns = { fn = fun state -> 
    let found = List.find_map parserfns ~f:(fun parser -> 
        match (maybe @@ parser()).fn state with
        | Ok(None, _) -> None
        | Ok(Some(v), state) -> Some(Ok(v, state))
        | Error e -> Some(Error(e))
    ) in
    match found with
    | Some(v) -> v
    | None -> (no_match "unable to match").fn state
}

let choice parsers = { fn = fun state -> 
    let found = List.find_map parsers ~f:(fun parser -> 
        match (maybe parser).fn state with
        | Ok(None, _) -> None
        | Ok(Some(v), state) -> Some(Ok(v, state))
        | Error e -> Some(Error(e))
    ) in
    match found with
    | Some(v) -> v
    | None -> (no_match "unable to match").fn state
}

(* TODO: single separated_by function *)
let separated_by_with_traliing sep p = 
    let separated_elem =
        let+ _ = sep 
        and+ elem = p 
        in elem
    in
    let+ first = p 
    and+ rest = many separated_elem in
        (first :: rest)

let separated_by sep p = 
    let separated_elem =
        let+ _ = sep 
        and+ elem = expect ~ctx: "sep" p 
        in elem
    in
    let+ first = p 
    and+ rest = many separated_elem in
        (first :: rest)

(* module Arg = struct 
    let rec ident() = 
        let+ ident = Lexemes.ident in
        Node.Arg.{span = ident.range; ident = ident.value}

    and tuple() = 
        let+ _ = Lexemes.open_paren 
        (* todo: annotate *)
        and+ elems = maybe @@ separated_by Lexemes.coma (arg ())
        and+ _ = expect 
            ~ctx: "tuple" 
            ~exp: "close brace" 
            @@ ignore_newline @@ Lexemes.close_paren in
        Node.Arg.{tuple = Option.value ~default: [] elems}

    and arg () = choicef [
        (fun () -> map (tuple()) (fun v -> Node.Arg.Tuple v));
        (fun () -> map (ident()) (fun v -> Node.Arg.Ident v))
    ]
end *)

let rec last_list_item = function
| [] -> raise (Invalid_argument "no arguments are provided")
| it :: [] -> it
| _ :: rest -> last_list_item rest

let merge_spanned fn = function
    | [] -> raise (Invalid_argument "the function accepts one or more arguments but none was provided")
    | item :: [] -> Span.{item with value = fn [item.value]}
    | head :: rest ->
        let values = List.map (head :: rest) 
            ~f:(fun spanned -> Span.(spanned.value)) 
        in Span.{
            value = fn values;
            range = {
                start = head.range.start;
                end' = (last_list_item rest).range.end'
            }
        }

let (pattern_block: unit -> Node.Match.cases Comb.t), set_pattern_block = forward_decl ()

module Type = struct 

    let rec typ' () = choicef [
        (fun () -> lambda ());
        (fun () -> simple ());
        (fun () -> tuple ());
        (fun () -> list ());
    ]
    and list () = 
        let+ _ = Lexemes.open_bracket
        and+ values = separated_by (Lexemes.coma) (typ' ())
        and+ _ = expect @@ ignore_newline @@ Lexemes.close_bracket in 
        let typ = match values with
            | [] -> (raise Common.Unreachable)
            | [typ] -> typ
            | items -> (Node.Type.Tuple { items })
        in Node.Type.List typ

    and simple () =
        let+ id = Lexemes.ident
        (* TODO: don't allow id args *)
        and+ args = many (param_typ ()) in
        if is_ident_uppercase id.value then 
            Node.Type.Simple {
                name = id;
                args = args; 
            }
        else Node.Type.Var {name = id}
    and lambda () =
        let+ arg = choicef [
            (fun () -> simple ());
            (fun () -> list ());
            (fun () -> tuple ())
        ]
        and+ _ = Lexemes.func_arrow
        and+ result = expect @@ ignore_newline @@ typ' () in
        Node.Type.Lambda {
            arg = arg;
            result = result;
        }
    and tuple () = 
        let+ _ = Lexemes.open_paren
        and+ values = maybe @@ separated_by (Lexemes.coma) (typ' ())
        and+ _ = expect @@ ignore_newline @@ Lexemes.close_paren in 
        match values with
        | None -> Node.Type.Unit
        | Some [n] -> n
        | Some items -> Node.Type.Tuple { items }

    and param_typ () = choicef [
        (fun () -> simple ());
        (fun () -> tuple ());
        (fun () -> list ());
    ]
end

let scheme = Type.typ'

module Typedef = struct 
    open Node

    let foreign = 
        let+ span = Lexemes.foreign in Typedef.Foreign (span.range)

    let typedef = 
        (* TODO: only uppercase  *)
        let param =
            let ident = Lexemes.ident
                |> validate (fun f -> if not @@ is_ident_uppercase Span.(f.value) then `Ok f else `SyntaxErr "type variable should start with a lowercase letter")
            in ident
        in
        let+ _ = Lexemes.type'
        and+ name = expect @@ ignore_newline @@ Lexemes.ident
            |> validate (fun f -> if is_ident_uppercase Span.(f.value) then `Ok f else `SyntaxErr "type name should start with an uppercase letter")
        and+ params = expect @@ ignore_newline @@ many @@ param
        and+ _ = expect @@ ignore_newline @@ Lexemes.eq
        and+ def = expect @@ ignore_newline @@ choice [foreign] in
        Typedef.{
            name;
            def;
            params = List.map params ~f: (fun var -> {var})
        }
end

module Destruct = struct 
    open Node.Destruct
    let rec name = let+ id = Lexemes.ident in (Name id)
    and tuple () =
        let+ _ = Lexemes.open_paren 
        and+ content = maybe @@ separated_by (ignore_newline @@ Lexemes.coma) (ignore_newline @@ (shape ()))
        and+ _ = Lexemes.close_paren in
        match content with
        | None -> Unit
        | Some (single :: []) -> single
        | Some multiple -> Tuple multiple
    and shape () = choicef [
        (fun () -> name); tuple;
    ]
end

module Param = struct 
    let block_optional ~expr = 
        let+ name = Lexemes.ident
        and+ _ = Lexemes.matc 
        and+ alias = maybe @@ (
            let+ _ = Lexemes.colon
            and+ alias = Lexemes.ident in alias)
        and+ default = maybe @@ (
            let+ _ = Lexemes.eq
            and+ e = expect @@ ignore_newline @@ (expr ()) in e
        ) in Node.Param.Optional {name; alias; default}

    let block_named = 
        let+ name = Lexemes.ident
        and+ shape = maybe @@ (
            let+ _ = Lexemes.colon
            and+ shape = (Destruct.shape ()) in shape
        ) in Node.Param.Named {name; shape}

    let block_param ~expr = choice [block_optional ~expr; block_named]

    let block ~expr = 
        let semi = 
            let+ _ = maybe newlines 
            and+ _ = Lexemes.semi
            and+ _ = maybe newlines in ()
        in
        let+ _ = Lexemes.open_block
        and+ params = expect @@ ignore_newline @@ separated_by_with_traliing (
            choice [ semi; newlines ]
        ) (block_param ~expr)
        and+ _ = expect @@ ignore_newline @@ Lexemes.close_block in
        params

    let extension = 
        let+ _ = Lexemes.spread
        and+ name = expect @@ Lexemes.ident
        and+ type_ident = expect @@ (
            let+ _ = Lexemes.colon
            and+ type_ident = (Type.param_typ ()) in type_ident
        ) in [Node.Param.Extension {name; type_ident}]

    let single_block_param ~expr = 
        let+ _ = Lexemes.open_paren
        and+ param = expect @@ ignore_newline @@ (block_param ~expr)
        and+ _ = expect @@ ignore_newline @@ Lexemes.close_paren in [param]

    let optional = 
        let+ name = Lexemes.ident
        and+ _ = Lexemes.matc 
        and+ alias = maybe @@ (
            let+ _ = Lexemes.colon
            and+ alias = Lexemes.ident in alias) 
        in [Node.Param.Optional {name; alias; default = None}]

    let named = 
        let+ name = Lexemes.ident
        and+ shape = maybe @@ (
            let+ _ = Lexemes.colon
            and+ shape = (Destruct.shape()) in shape) 
        in [Node.Param.Named {name; shape}]

    let non_positional ~expr = 
        let+ _ = Lexemes.ampersand 
        and+ value = choice [
            block ~expr; extension; optional; named; single_block_param ~expr
        ] in value

    let positional = 
        let+ shape = (Destruct.shape ()) in Node.Param.Positional {shape}

    let param ~expr = choicef [
        (fun () -> map positional (fun p -> [p]));
        (fun () -> non_positional ~expr);
    ]
end

let using = 
    let rename = 
        let+ _ = Lexemes.ident_of ["as"]
        and+ ident = expect ~exp: "identifier" @@ Lexemes.ident 
        (* TODO: validate case when renaming, ident shouldn't be qualified *)
        in ident
    in

    let rec nested_name () = 
        let wildcard = 
            let+ op = Lexemes.operator_of ["*"] in
                Node.Using.Wildcard (op)
        in
        let ident = 
            let+ name = Lexemes.ident
            and+ action = maybe @@ choicef [
                (fun () -> map rename (fun ident -> Node.Using.Rename ident));
                (fun () -> map (nested_block ()) (fun names -> Node.Using.Nested names));
            ] in
            Node.Using.Ident {
                name = name;
                action = action 
            }
        in 
        choice [wildcard; ident]
    and nested_block () =
        let+ _ = Lexemes.open_block
        and+ names = separated_by (choice [
            map Lexemes.coma (fun _ -> ());
            map (one_more Lexemes.line_break) (fun _ -> ()); 
        ]) (ignore_newline @@ (nested_name ()))
        and+ _ = expect @@ ignore_newline @@ Lexemes.close_block
        in names
    in
    let+ keyword = Lexemes.import
    and+ kind = choice [
        map Lexemes.str  (fun global -> Node.Using.Global global);
        map Lexemes.ident (fun local -> Node.Using.Local local);
    ]
    and+ action = choicef [
        (fun () -> map rename (fun ident -> Node.Using.Rename ident));
        (fun () -> map (nested_block()) (fun names -> Node.Using.Nested names))
    ] in
    Node.Using.{
        keyword; 
        kind;
        action;
    }
    
let allow_operator_wrap op = ["-"; "+"] 
    |> List.find ~f: (String.equal op) 
    |> Option.is_none

let combine_if bool parser rest =
    if bool then parser @@ rest else rest

let operator_ident = 
    let+ _ = Lexemes.open_paren
    and+ op = Lexemes.operator 
    and+ _ = Lexemes.close_paren
    in op

let foreign = 
    let+ _ = Lexemes.foreign 
    and+ name = expect @@ ignore_newline @@ Lexemes.str 
    and+ typ = expect @@ ignore_newline @@ (Type.param_typ ())
    in (Node.Value.Foreign Node.Foreign.{
        name = name;
        typ = typ;
    }) 
let rec value () = choice [
    map Lexemes.int (fun i -> Node.Value.Int i);
    map Lexemes.str (fun s -> Node.Value.Str s);
    map Lexemes.ident (fun id -> Node.Value.Ident id);
    map (lambda()) (fun v -> Node.Value.Lambda v);
    map (list()) (fun v -> Node.Value.Li v);
    foreign
] 
(*
let x = struct &{
    name: 10 &value
}
pun
group
value
*)

(* TODO: check if it's possible to swith monads to applicatives whenever it's possible *)
and cond () =
    let block_or_expr () = choice [
        (* TODO: rename to expr *)
        map (block ()) (fun n -> Node.Cond.Block n);
        map (expr ()) (fun n -> Node.Cond.Expr n);
    ] in
    let* _ = Lexemes.if' in
    let* if_expr = expect 
        ~ctx:"conditional expression" 
        ~exp: "an expression or a statement block" 
        @@ ignore_newline @@ block_or_expr () in
    let* _ = expect 
        ~ctx:"then" 
        @@ ignore_newline @@ Lexemes.then' in
    let* then_expr = expect 
        ~ctx:"conditional expression" 
        ~exp: "an expression or a statement block" 
        @@ ignore_newline @@ block_or_expr () in
    let* else_ = maybe 
        @@ ignore_newline @@ Lexemes.else' in
    match else_ with 
        | None -> 
            let if_range = (Node.Cond.expr_range if_expr) in
            let then_range = (Node.Cond.expr_range then_expr) in
            return Node.Cond.{
                range = Span.merge if_range then_range;
                if_=if_expr;
                then_=then_expr;
                else_=None
            }
        | Some _ ->
            let* else_expr = expect 
                ~ctx:"conditional expression" 
                ~exp: "an expression or a statement block" 
                @@ ignore_newline @@ block_or_expr () in
            let if_range = (Node.Cond.expr_range if_expr) in
            let else_range = (Node.Cond.expr_range else_expr) in
            return Node.Cond.{
                range = Span.merge if_range else_range;
                if_=if_expr;
                then_=then_expr;
            else_=Some else_expr
                }
and fn_expr () =
    let parens () = 
        let+ open_paren = Lexemes.open_paren 
        and+ expr = maybe @@ (tuple_expr ())

        and+ close_paren = expect @@ ignore_newline @@ Lexemes.close_paren in
        match expr with
        | None -> Node.Expr.Value (Node.Value.Unit (Span.merged open_paren.range close_paren.range ()))
        | Some v -> v
    in 
    (* elem *)

    choicef [
        (fun () -> parens ());
        (fun () -> map (value ()) (fun v -> Node.Expr.Value v))
    ]
and fn_arg () = 
    let named =
        let arg =
            let+ name = Lexemes.ident
            and+ e = maybe @@ (
                let+ _ = Lexemes.colon
                and+ e = ignore_newline @@ fn_expr() in e
            ) in match e with 
                | Some expr -> Node.Apply.NameArg {name; expr}
                | None -> Node.Apply.PunArg {name}
        in
        let block = 
            let semi = 
                let+ _ = maybe newlines 
                and+ _ = Lexemes.semi
                and+ _ = maybe newlines in ()
            in
            let+ _ = Lexemes.open_block
            and+ args = expect @@ ignore_newline @@ separated_by_with_traliing (
                choice [ semi; newlines ]
            ) arg
            and+ _ = expect @@ ignore_newline @@ Lexemes.close_block
            in args
        in
        let+ _ = Lexemes.ampersand
        and+ value = choice [
            block; 
            map arg (fun arg -> [arg])
        ] in value
    in
    let unnamed =
        map (fn_expr ()) (fun expr -> [Node.Apply.PosArg {expr}])
    in choice [unnamed; named]
and fn () = 
    let+ cell = fn_expr() 
    (* TODO: parens and expressions here!!! *)
    and+ args = many (fn_arg ()) in
        match args with
        | [] -> cell 
        | some -> 
            let args = Util.Lists.flatten some in
            Node.Expr.Apply Node.Apply.{
                fn = cell;
                args = args;
                range = Span.merge (Node.Expr.range cell) (List.last_exn args |> Node.Apply.arg_range)
            }

and list () = 
    let semi = 
        let+ _ = maybe newlines 
        and+ _ = Lexemes.semi
        and+ _ = maybe newlines in ()
    in
    let rec single () =  
        let+ e = expr () 
        and+ rest = maybe @@ next () in
        match rest with
        | None -> [Node.Li.Single e]
        | Some items -> ((Node.Li.Single e) :: items)
    and spread () = 
        let+ _ = Lexemes.spread
        and+ e = expect @@ expr ()
        and+ rest = maybe @@ next () in
        match rest with
        | None -> [Node.Li.Spread e]
        | Some items -> ((Node.Li.Spread e) :: items)
    and next () = 
        let next_single () = 
            let+ _ = choice [semi; newlines] 
            and+ r = single () in r
        in
        let next_spread () = 
            let+ _ = maybe @@ choice [semi; newlines] 
            and+ r = spread () in r
        in choicef [(fun () -> next_single()); (fun () -> next_spread())]
    in
    let init = choicef [
        (fun () -> single ());
        (fun () -> spread ())
    ] in
    let+ open_bracket = Lexemes.open_bracket 
    and+ exprs = ignore_newline @@ maybe @@ init
    and+ close_bracket = expect @@ ignore_newline @@ Lexemes.close_bracket in
    Node.Li.{
        range = Span.merge open_bracket.range close_bracket.range;
        items = exprs |> Option.value ~default: []
    }

and unary () =
    let rec make expr = function
    | [] -> expr
    | v :: rest ->
        let op_fn = Node.Expr.Value (Node.Value.Ident v) in
        let arg = make expr rest in
        Node.Expr.Apply Node.Apply.{
            range=(Span.merge v.range (Node.Expr.range arg));
            fn=op_fn;
            (* TODO: use "unary" *)
            args=[
                Node.Apply.PosArg {expr = Node.Expr.Value (Node.Value.Int (Span.from (Span.empty_range) "0"))};
                Node.Apply.PosArg {expr = arg}
            ]
        }
    in
    let+ ops = many 
        @@ Lexemes.operator_of ["-"]
    and+ ex  = fn()
    and+ matc = maybe @@ (
        let+ _ = Lexemes.matc
        and+ block = ignore_newline @@ expect @@ pattern_block () in block
    ) in
    let expr = make ex ops in
    match matc with
    | None -> expr
    | Some cases -> Node.Expr.Match (Node.Match.{
        expr;
        range = Span.merge (Node.Expr.range expr) (cases.range);
        block = cases
    }) 
and binary' = function
 | [] -> unary ()
 | precedence :: higher ->
    let rec loop result =
        let* lexeme = maybe @@ choice [
            ignore_newline 
                @@ Lexemes.operator_of (List.filter precedence ~f: allow_operator_wrap);
            Lexemes.operator_of precedence
        ]
        in 
        match lexeme with
        | None -> return (result)
        | Some (lexeme) -> 
            let* right = binary' higher in 
            loop @@ Node.Expr.Apply Node.Apply.{
                range = (Span.merge (Node.Expr.range result) (Node.Expr.range right));
                fn=(Node.Expr.Value (Node.Value.Ident lexeme));
                args=[Node.Apply.PosArg{expr=result}; Node.Apply.PosArg{expr=right}]
            }
    in
    let* left = binary' higher in
    let* result = loop left in 
        return result
and binary () = binary' precedence
and tuple_expr () =
    let elem = choicef [
        (fun () -> map (binary' precedence) (fun v -> v));
        (fun () -> map (cond()) (fun v -> Node.Expr.Cond v));
    ] in
    let+ first = separated_by (ignore_newline @@ Lexemes.coma) (ignore_newline @@ elem) in
    match first with
    | [] -> (raise (Common.Unreachable))
    | only :: [] -> only
    | first :: last -> Node.Expr.Value (Node.Value.Tuple {
        exprs = first :: last;
        range = Span.merge (Node.Expr.range first) (Node.Expr.range (List.last_exn last))
    })
and expr () = tuple_expr ()
    (* let parens = 
        let+ open_paren = Lexemes.open_paren 
        and+ expr = maybe @@ (tuple_expr ())
        and+ close_paren = expect @@ ignore_newline @@ Lexemes.close_paren in
        match expr with
        | None -> Node.Expr.Value (Node.Value.Unit (Span.merged open_paren.range close_paren.range ()))
        | Some v -> v
    in 
    choice [ 
        parens; (tuple_expr());
    ] *)
(* and tuple () =
    let+ op = Lexemes.open_paren 
    (* TODO: separated_by (,) *)
    and+ expr = maybe @@ expr () 
    and+ close = expect ~ctx: "tuple" 
        @@ ignore_newline 
        @@ Lexemes.close_paren in 
    match expr with
    | None -> Node.Expr.Value (Node.Value.Unit (
        Span.from (Span.merge op.range close.range) ()))
    | Some e -> e *)
and sig_ () =
    let+ _ = Lexemes.sig'
    and+ typ = expect @@ Type.typ' () 
    and+ let' = expect @@ ignore_newline @@ let_ () in
    Node.Let.{let' with sig' = Some typ}
and let_ () = 
    let let_rhs = choicef [
        (fun () -> map (block()) (fun v -> Node.Let.Block v));
        (fun () -> map (expr()) (fun v -> Node.Let.Expr v));
    ] in
    let let_ident = choice [
        Lexemes.ident; operator_ident
    ] in
    let+ keyword = Lexemes.let'
    and+ rec_ = maybe @@ Lexemes.rec_
    and+ ident = expect 
        ~ctx:"let" 
        ~exp:"identifier" 
        @@ ignore_newline @@ let_ident
    and+ params = many @@ (Param.param ~expr)
    and+ _ = expect 
        ~ctx:"let" 
        ~exp:"=" 
        @@ ignore_newline @@ Lexemes.eq
    and+ ex = expect ~ctx: "le rhs" 
        @@ ignore_newline @@ let_rhs in
    let param_list = match Util.Lists.flatten params with 
        | [] -> None 
        | params -> Some params
    in Node.Let.{
        sig' = None;
        range = Span.merge keyword.range (Node.Let.expr_range ex);
        is_rec = Option.is_some rec_;
        params = param_list;
        ident = ident;
        expr = ex
    }
and block () = 
    let+ open_lex = Lexemes.open_block
    and+ stmts = block_stmts() 
    and+ close_lex = expect 
        (* TODO: why *)
        (* ~exp: "block statement or end of block" *)
        @@ ignore_newline @@ Lexemes.close_block
    in Node.Block.{
        range = Span.merge open_lex.range close_lex.range;
        stmts=stmts
    }

and block_stmt () = 
    let stmt () = choicef [
        (fun () -> map (sig_()) (fun v -> Node.Block.Let v));
        (fun () -> map (let_()) (fun v -> Node.Block.Let v));
        (fun () -> map (block ()) (fun v -> Node.Block.Block v));
        (fun () -> map (expr ()) (fun v -> Node.Block.Expr v));
        (fun () -> map (using) (fun v -> Node.Block.Using v));
    ] in 
        let+ s = ignore_newline @@ (stmt ())
        and+ _ = maybe @@ Lexemes.stmt_separator in 
        s

and block_stmts () = one_more (block_stmt()) (*(Comb.wrap_fn block_stmt)*)

and lambda () =
    let+ start = Lexemes.lambda
    and+ params = many @@ (ignore_newline @@ Param.param ~expr)
    and+ b = expect @@ ignore_newline @@ (block ()) in
        Node.Lambda.{
            range=Span.merge start.range b.range;
            params = Util.Lists.flatten params;
            block=b
        }

let type_decl = 
    let+ span = Lexemes.foreign in
    `Typedef (Node.Typedef.Foreign span.range)

let rec typespace_name_decl = 
    let+ name = Lexemes.ident (* TODO: it should be uppercase *)
    (* TODO: type parameters *)
    and+ _ = Lexemes.eq
    and+ value = expect ~exp: "TODO" @@ ignore_newline @@ choice [type_decl]
    in match value with
        | `Typedef def -> Node.Module.Typedef Node.Typedef.{
            name = name;
            params = [];
            def = def;
        }

and module_entries () = 
    let root_stmt () = choicef [
        (fun () -> map (sig_()) (fun v -> Node.Module.Let v));
        (fun () -> (map Typedef.typedef) (fun v -> Node.Module.Typedef v));
        (fun () -> map (let_()) (fun v -> Node.Module.Let v));
        (fun () -> map (using) (fun v -> Node.Module.Using v));
        (fun () -> map (modu ()) (fun m -> Node.Module.Module m));
        (fun () -> typespace_name_decl);
    ] in let separated_block_stmt = 
        let+ stmt = ignore_newline @@ (root_stmt ())
        and+ _ = maybe @@ Lexemes.stmt_separator in
            stmt
    in many separated_block_stmt

let () = set_pattern_block (fun () -> 
    let open Node in
    let param = map Lexemes.ident (fun id -> Match.Param id) in
    let int = map Lexemes.int (fun id -> Match.Int id) in
    let str = map Lexemes.str (fun id -> Match.Str id) in
    let (parens: unit -> Match.pattern Comb.t), set_parens = forward_decl () in
    let (elem: unit -> Match.pattern Comb.t), set_elem = forward_decl () in
    let (tuple: unit -> Match.pattern Comb.t), set_tuple = forward_decl () in
    let list = Comb.wrap_fn (fun () ->
        let+ _ = Lexemes.open_bracket
        and+ items = maybe @@ separated_by (ignore_newline @@ Lexemes.semi) (wrap_fn tuple)
        and+ rest = maybe @@ ignore_newline @@ (
            let+ _ = Lexemes.spread
            and+ id = expect @@ param in id
        )
        and+ _ = expect @@ Lexemes.close_bracket in
        Match.List { items = Option.value ~default: [] items; rest }
    ) in
    set_parens (fun () ->
        let+ _ = Lexemes.open_paren
        and+ e = expect @@ Comb.wrap_fn tuple
        and+ _ = expect @@ Lexemes.close_paren in
        e
    );
    set_elem (fun () ->
        choice [param; int; str; list; parens ()]
    );
    set_tuple (fun () ->
        let+ elems = separated_by (ignore_newline @@ Lexemes.coma) (ignore_newline @@ elem ()) in
        match elems with
        | [] -> raise (Common.TODO)
        | only :: [] -> only
        | multiple -> Match.Tuple multiple
    );

    let case = 
        let+ _ = Lexemes.pipe
        and+ pattern = expect @@ tuple ()
        and+ _ = expect @@ Lexemes.func_arrow
        and+ stmts = expect @@ one_more @@ (Comb.wrap_fn block_stmt) in 
        Match.{pattern; stmts}
    in
    let+ op = Lexemes.open_block
    and+ cases = expect @@ ignore_newline @@ one_more case
    and+ cl = expect @@ ignore_newline @@ Lexemes.close_block in
    Match.{range = Span.merge op.range cl.range; cases}
)

let () = init_modu (fun () ->
    let+ keyword = Lexemes.modu
    and+ name = expect @@ Lexemes.ident
    and+ _ = expect @@ ignore_newline @@ Lexemes.eq
    and+ _ = expect @@ ignore_newline @@ Lexemes.open_block
    and+ entries = (module_entries ())
    and+ close_paren = expect @@ ignore_newline @@ Lexemes.close_block
    in Node.Module.{
        keyword;
        range = Span.merge keyword.range close_paren.range;
        name = name;
        entries
    }
)



let root = 
    let+ entries = module_entries ()
    and+ _ = expect 
        ~exp: "toplevel statement" 
        @@ ignore_newline @@ eof in
    Node.Root.{
        entries
    }