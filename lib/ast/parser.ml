open Base
open Common

open Comb
open Comb.Syntax

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
end

let throw e = {fn = fun _ -> Error e}

let precedence = [ ["|>"]; ["$"]; ["+"; "-"]; ["*"; "/"; "%"]; [">"; "<"; "=="; "!="]]

let wrap p = {fn = p.fn}

let newlines = let+ _ = one_more @@ Lexemes.line_break in ()

let ignore_newline p = 
    let+ _ = many @@ Lexemes.line_break 
    and+ v = p in v

let expect ?ctx ?exp p =  { fn = fun state ->
    match p.fn state with
    | Ok k -> Ok k
    | Error e -> 
        if e.no_match then 
            let lex_str = e.caused_by |> Lexeme.to_string in
            let unexp_str = "unexpected \"" ^ lex_str ^ "\"" in
            let ctx_str = match ctx with | Some ctx -> "while " ^ ctx | None -> "" in
            let exp_str = match exp with | Some exp -> "expecting " ^ exp | None -> "" in
            let total = [unexp_str; ctx_str; exp_str] 
                |> List.filter ~f: (fun str -> not @@ String.is_empty str) 
                |> String.concat ~sep:" " in
            Error {e with no_match = false; err_msg = total}
        else Error e
}

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

module Arg = struct 
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
end

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

let type' =
    let rec typ' () = choicef [
        (fun () -> lambda ());
        (fun () -> simple ());
        (fun () -> tuple ());
    ]
    and simple () =
        let+ id = Lexemes.ident 
        and+ args = many (*type'*) Lexemes.ident in
        Node.Type.Simple {
            name = id;
            args = List.map args ~f:(fun arg -> 
                Node.Type.Simple {name = arg; args = []})
        }
    and lambda () =
        let+ arg = choicef [
            (fun () -> simple ());
            (fun () -> tuple ())
        ]
        and+ _ = Lexemes.func_arrow
        and+ result = expect @@ typ' () in
        Node.Type.Lambda {
            arg = arg;
            result = result;
        }
    and tuple () = 
        let+ _ = Lexemes.open_paren
        and+ values = separated_by (Lexemes.coma) (typ' ())
        and+ _ = expect @@ Lexemes.close_paren in 
        match values with
        | [] -> Node.Type.Unit
        | [n] -> n
        | items -> Node.Type.Tuple { items }
    in typ'

let scheme = type'

let import = 
    let rename = 
        let+ _ = Lexemes.ident_of ["as"]
        and+ ident = expect ~exp: "identifier" @@ Lexemes.ident
        in ident
    in

    let rec nested_name () = 
        let+ ident = Lexemes.ident
        and+ modifier = maybe @@ choicef [
            (fun () -> map rename (fun ident -> `Rename ident));
            (fun () -> map (nested_block ()) (fun names -> `Nested names));
        ] in
        Node.Import.{
            name = ident;
            rename = begin match modifier with
                | Some `Rename ident -> Some ident
                | _ -> None 
            end;
            nested = match modifier with
                | Some `Nested names -> Some names
                | _ -> None
        }
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
    and+ source = Lexemes.str 
    and+ modifier = maybe @@ choicef [
        (fun () -> map rename (fun ident -> `Rename ident));
        (fun () -> map (nested_block()) (fun names -> `Nested names))
    ] in
    Node.Import.{
        keyword; 
        source = 
        Node.Import.{ name = source;
            rename = begin match modifier with
                | Some `Rename ident -> Some ident
                | _ -> None
            end;
            nested = match modifier with
                | Some `Nested names -> Some names
                | _ -> None
        }
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
    and+ name = Lexemes.str 
    in (Node.Value.Foreign name) 
let rec value () = choice [
    map Lexemes.int (fun i -> Node.Value.Int i);
    map Lexemes.str (fun s -> Node.Value.Str s);
    map Lexemes.ident (fun id -> Node.Value.Ident id);
    map (lambda()) (fun v -> Node.Value.Lambda v);
    map (list()) (fun v -> Node.Value.Li v);
    foreign
] 
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
and fn_arg () =
    (* Common.log [Common.stacktrace ()]; *)
    let parens () = 
        let+ open_paren = Lexemes.open_paren 
        and+ expr = maybe @@ (tuple_expr ())
        and+ close_paren = expect @@ Lexemes.close_paren in
        match expr with
        | None -> Node.Expr.Value (Node.Value.Unit (Span.merged open_paren.range close_paren.range ()))
        | Some v -> v
    in 
    (* elem *)

    choicef [
        (fun () -> parens ());
        (fun () -> map (value ()) (fun v -> Node.Expr.Value v))
    ]
and fn () = 
    let+ cell = fn_arg() 
    (* TODO: parens and expressions here!!! *)
    and+ args = many (fn_arg ()) in
        match args with
        | [] -> cell 
        | some -> Node.Expr.Apply Node.Apply.{
            fn = cell;
            args = some;
            range = Span.merge (Node.Expr.range cell) (Node.Expr.range (List.last_exn some))
        }
and list () = 
    let semi = 
        let+ _ = maybe newlines 
        and+ _ = Lexemes.semi
        and+ _ = maybe newlines in ()
    in
    let+ open_bracket = Lexemes.open_bracket 
    and+ exprs = ignore_newline @@ maybe @@ separated_by_with_traliing (
        choice [ 
            semi; newlines
        ]
    ) (expr())
    and+ close_bracket = expect @@ ignore_newline @@ Lexemes.close_bracket in
    Node.Li.{
        range = Span.merge open_bracket.range close_bracket.range;
        items = exprs |> Option.value ~default: []
    }
(* and expr () = 
    (* let elem = choicef [
    (* use here the .fn = fn trick *)
        (fun () -> map (value ()) (fun n -> Node.Expr.Value n));
        (fun () -> map (tuple' ()) (fun n -> n));
        (fun () -> map (cond ()) (fun n -> Node.Expr.Cond n));
    ] in  *)
    let parens = 
        let+ open_paren = Lexemes.open_paren 
        and+ expr = maybe @@ (binary ())
        and+ close_paren = expect @@ Lexemes.close_paren in
        match expr with
        | None -> Node.Expr.Value (Node.Value.Unit (Span.merged open_paren.range close_paren.range ()))
        | Some v -> v
    in 
    (* elem *)

    choice [
        (binary()); parens
    ] *)
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
            args=[Node.Expr.Value (Node.Value.Int (Span.from (Span.empty_range) "0")); arg]
        }
    in
    let+ ops = many 
        @@ Lexemes.operator_of ["-"]
    and+ ex = fn () in make ex ops
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
                args=[result; right]
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
and expr () = 
    let parens = 
        let+ open_paren = Lexemes.open_paren 
        and+ expr = maybe @@ (tuple_expr ())
        and+ close_paren = expect @@ ignore_newline @@ Lexemes.close_paren in
        match expr with
        | None -> Node.Expr.Value (Node.Value.Unit (Span.merged open_paren.range close_paren.range ()))
        | Some v -> v
    in 
    choice [ 
        parens; (tuple_expr());
    ]
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
    and+ typ = expect @@ type' () 
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
    and+ args = many @@ (Arg.arg ())
    and+ _ = expect 
        ~ctx:"let" 
        ~exp:"=" 
        @@ ignore_newline @@ Lexemes.eq
    and+ ex = expect ~ctx: "le rhs" 
        @@ ignore_newline @@ let_rhs in
    let arg_list = match args with 
        | [] -> None 
        | args -> Some Node.Arg.{args=args} 
    in Node.Let.{
        sig' = None;
        range = Span.merge keyword.range (Node.Let.expr_range ex);
        is_rec = Option.is_some rec_;
        args = arg_list;
        ident = ident;
        expr = ex
    }
and block () = 
    let+ open_lex = Lexemes.open_block
    and+ stmts = block_stmts() 
    and+ close_lex = expect 
        ~ctx: "close block" 
        @@ ignore_newline @@ Lexemes.close_block
    in Node.Block.{
        range = Span.merge open_lex.range close_lex.range;
        stmts=stmts
    }

and block_stmts () = 
    let block_stmt () = choicef [
        (fun () -> map (sig_()) (fun v -> Node.Block.Let v));
        (fun () -> map (let_()) (fun v -> Node.Block.Let v));
        (fun () -> map (block ()) (fun v -> Node.Block.Block v));
        (fun () -> map (expr ()) (fun v -> Node.Block.Expr v));
    ] in let separated_block_stmt = 
        let+ stmt = ignore_newline @@ (block_stmt ())
        and+ _ = maybe @@ Lexemes.stmt_separator in stmt
    in many Comb.{fn = separated_block_stmt.fn}

and lambda () =
    let+ start = Lexemes.lambda
    and+ args = many @@ (Arg.arg ())
    and+ b = (block ()) in
        Node.Lambda.{
            range=Span.merge start.range b.range;
            args={args=args};
            block=b
        }

and module_entries () = 
    let root_stmt () = choicef [
        (fun () -> map (sig_()) (fun v -> Node.Module.Let v));
        (fun () -> map (let_()) (fun v -> Node.Module.Let v));
        (fun () -> map (import) (fun v -> Node.Module.Import v));
    ] in let separated_block_stmt = 
        let+ stmt = ignore_newline @@ (root_stmt ())
        and+ _ = maybe @@ Lexemes.stmt_separator in
            stmt
    in many separated_block_stmt

let root = 
    let+ entries = module_entries ()
    and+ _ = expect 
        ~exp: "toplevel statement or end of file" 
        @@ ignore_newline @@ eof in
    Node.Root.{
        entries
    }