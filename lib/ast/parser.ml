open Parlex
open Base

open Comb
open Comb.Syntax

exception Todo of string

let make_state = State.make

let let_lexeme = one_value (function Lexeme.Let -> Some () | _ -> None)

let ident_lexeme = 
    let* id = one_value (function Lexeme.Ident value -> Some value | _ -> None) in
    return Node.{ ident_pos = id.start_pos; ident = id.value }

let int_lexeme = 
    let* i = one_value (function Lexeme.Int int -> Some int | _ -> None) in 
    return Node.{int_pos = i.start_pos; int = i.value}


let int = one (fun lexeme -> 
    match lexeme.value with
    | Lexeme.Int int -> Some (Node.{int_pos = lexeme.start_pos; int = int})
    | _ -> None
)

let str = one (fun lexeme -> 
    match lexeme.value with
    | Lexeme.Str str -> Some (Node.{str_pos = lexeme.start_pos; str = str})
    | _ -> None
)

let ident = one (fun lexeme -> 
    match lexeme.value with
    | Lexeme.Ident ident -> Some (Node.{ident_pos = lexeme.start_pos; ident = ident})
    | _ -> None
)

let throw e = {fn = fun _ -> Error e} 

let precedence = [ ["$"]; ["|>"]; ["+"; "-"]; ["*"; "/"]; [">"; "<"] ]

let wrap p = {fn = p.fn}

let ignore_newline p =
    let+ _ = many @@ one_value (function | Lexeme.LineBreak -> Some() | _ -> None) 
    and+ v = p in v

let expect ?ctx ?exp p =  { fn = fun state ->
    match p.fn state with
    | Ok k -> Ok k
    | Error e -> 
        if e.no_match then 
            let lex_str = e.caused_by |> Lexeme.to_string in
            let unexp_str = "unexpected " ^ lex_str in
            let ctx_str = match ctx with | Some ctx -> "while " ^ ctx | None -> "" in
            let exp_str = match exp with | Some exp -> "expecting " ^ exp | None -> "" in
            let total = [unexp_str; ctx_str; exp_str] |> String.concat ~sep:" " in
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

let separated_by sep p = 
    let separated_elem =
        let+ _ = sep 
        and+ elem = expect p in elem
    in
    let+ first = p 
    and+ rest = many separated_elem in
        (first :: rest)

module Arg = struct 
    (* TODO: simplify *)

    let rec ident() = 
        let+ ident = one_value (function | Lexeme.Ident id -> Some id | _ -> None) in
        Node.Arg.{ident_pos = ident.start_pos; ident = ident.value}
    and tuple() = 
        let+ _ = one_value (function | Lexeme.OpenParen -> Some () | _ -> None)
        (* todo: annotate *)
        and+ elems = maybe @@ separated_by 
            (one_value (function | Lexeme.Coma -> Some () | _ -> None))
            (arg ())
        and+ _ = expect @@ ignore_newline @@ one_value (function | Lexeme.CloseParen -> Some () | _ -> None) in
        Node.Arg.{tuple = Option.value ~default: [] elems}
    and arg () = choicef [
        (fun () -> map (tuple()) (fun v -> Node.Arg.Tuple v));
        (fun () -> map (ident()) (fun v -> Node.Arg.Ident v))
    ]
end


let rec value () = choice [
    map int (fun i -> Node.IntValue i);
    map str (fun s -> Node.StrValue s);
    map ident (fun id -> Node.IdentValue id);
    map (lambda()) (fun v -> Node.LambdaValue v);
] 
and block_or_expr () = choice [
    (* TODO: rename to expr *)
    map (block ()) (fun n -> Node.Block n);
    map (binary ()) (fun n -> Node.Expr n);
]
(* TODO: check if it's possible to swith monads to applicatives whenever it's possible *)
and cond () =
    let* _ = one_value(fun v -> match v with If -> Some() | _ -> None) in
    let* if_expr = expect ~ctx:"conditional expression" ~exp: "an expression or a statement block" @@ ignore_newline @@ block_or_expr () in
    let* _ = expect @@ ignore_newline @@ one_value(fun v -> match v with Then -> Some() | _ -> None)  in
    let* then_expr = expect ~ctx:"conditional expression" ~exp: "an expression or a statement block" @@ ignore_newline @@ block_or_expr () in
    let* else_ = maybe @@ ignore_newline @@ one_value(fun v -> match v with Else -> Some() | _ -> None)  in
    match else_ with 
        | None -> 
            return Node.{cond_expr=if_expr; cond_then=then_expr; cond_else=None}
        | Some _ ->
            let* else_block = expect ~ctx:"conditional expression" ~exp: "an expression or a statement block" @@ ignore_newline @@ block_or_expr () in
            return Node.{cond_expr=if_expr; cond_then=then_expr; cond_else=Some else_block}
and fn () = 
    let+ v = parens ()
    (* TODO: parens and expressions here!!! *)
    and+ args = many (parens()) in
        match args with
        | [] -> v
        (* TODO: wtf map *)
        | e -> Node.ApplyExpr Node.{apply_fn = v; apply_args = e}
and parens () = choice [
    map (value ()) (fun n -> Node.ValueExpr n);
    map (tuple ()) (fun n -> n);
    map (cond ()) (fun n -> Node.CondExpr n);
]
and unary () =
    let open Node in
    let+ ops = many @@ one_value (fun v -> match v with Op "-" -> Some("-") | _ -> None)
    and+ value = fn () in
    List.fold ops ~init:(value) ~f: (fun expr op ->
        ApplyExpr {
            apply_fn=(ValueExpr (IdentValue {ident_pos = op.start_pos; ident = op.value}));
            apply_args=[expr]
        }
    )
and binary' = function
 | [] -> unary ()
 | curr :: rest ->
    let open Node in
    let rec loop result =
        let* lexeme = maybe @@ one_value (function 
            | Op op -> if List.exists curr ~f: (String.equal op) then Some (op) else None
            | _ -> None
        ) in match lexeme with
            | None -> return (result)
            | Some (lexeme) -> 
                let* right = binary' rest in 
                loop @@ ApplyExpr {
                    apply_fn=(ValueExpr (IdentValue {ident_pos = lexeme.start_pos; ident = lexeme.value}));
                    apply_args=[result; right]
                }
    in
    let* left = binary' rest in
    let* result = loop left in 
        return result
and binary () = binary' precedence
and tuple () =
    let* _ = one_value (function | OpenParen -> Some() | _ -> None) in
    let* expr = maybe @@ binary () in
    let* _ = expect @@ ignore_newline @@ one_value (function | CloseParen -> Some() | _ -> None) in 
    match expr with
    | None -> raise (Todo "handle units")
    | Some e -> return e
and let_ () = 
    let let_rhs = choice [
        map (block()) (fun v -> Node.LetBlock v);
        map (binary()) (fun v -> Node.LetExpr v);
    ] in
    let* keyword = let_lexeme in
    let* ident = expect ~ctx:"let" ~exp:"identifier" @@ ignore_newline @@ ident_lexeme in
    let* args = many @@ (Arg.arg ()) in
    let* _ = expect ~ctx:"let" ~exp:"=" @@ ignore_newline @@ one_value (function Lexeme.Eq -> Some () | _ -> None) in 
    let* ex = expect @@ ignore_newline @@ let_rhs in
    let arg_list = match args with | [] -> None  | args -> Some Node.Arg.{args=args} in
        return Node.{let_pos = keyword.start_pos; let_args = arg_list; let_ident = ident; let_expr = ex}
and block () = 
    let+ open_lex = one_value (function | Lexeme.OpenBlock -> Some() | _ -> None)
    and+ stmts = block_stmts() 
    and+ close_lex = expect @@ ignore_newline @@ one_value (function | Lexeme.CloseBlock -> Some() | _ -> None) in
    Node.{block_start_pos = open_lex.start_pos; block_end_pos = close_lex.end_pos; block_stmts=stmts}
and block_stmts () = 
    let block_stmt () = choicef [
        (fun () -> map (let_()) (fun v -> Node.LetStmt v));
        (fun () -> map (block ()) (fun v -> Node.BlockStmt v));
        (fun () -> map (binary()) (fun v -> Node.ExprStmt v));
    ] in let separated_block_stmt = 
        let+ stmt = ignore_newline @@ (block_stmt ())
        and+ _ = maybe @@ one_value (function | LineBreak -> Some() | Semi -> Some() | _ -> None) in
            stmt
    in many Comb.{fn = separated_block_stmt.fn}
and lambda () =
    let+ _ = one_value(function | Lexeme.Lambda -> Some () | _ -> None )
    and+ args = many @@ (Arg.arg ())
    and+ b = (block ()) in
        (* let foo = Option.value ~default:Node.{params=[]} args in *)
        Node.{lambda_args={args=args}; lambda_block=b}

let parse_root_stmts = 
    let* value = block_stmts () in
    let* _ = expect @@ eof in
    return value