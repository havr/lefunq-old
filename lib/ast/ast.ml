
module Node = Node
module Parser = Parser
module Lexeme = Lexeme
module Print = Print
module Scanner = Scanner
module Comb = Comb

open Common

let from_parlex_pos pos = {
    Pos.row = Parlex.Pos.(pos.row);
    col = Parlex.Pos.(pos.col)
}

let from_comb_err file err = Err.{
    context = None;
    file = file;
    msg = Comb.(err.err_msg);
    pos = from_parlex_pos @@ Comb.(err.err_pos)
}

let from_lexer_err file err = Err.{
    context = None;
    file = file;
    msg = Parlex.Lexer.Err.(err.msg);
    pos = from_parlex_pos @@ Parlex.Lexer.Err.(err.pos)
}

let of_lexemes ~file lexemes = 
    let parser_state = Comb.State.make lexemes in
    match Parser.parse_root_stmts.fn parser_state with
    | Error e -> Error (from_comb_err file e)
    | Ok (result, _) -> Ok result

let of_string ~file str = 
    match Scanner.scan_all str with
    | Error err -> Error (from_lexer_err file err)
    | Ok lexemes -> of_lexemes ~file lexemes
    
(* 

 - Parpar
should export:
 - Node
 - 
 - Lexeme
 - Scanner

 of_lexemes
 of_string
 *)




(* TODO: expose only nodes and Parse *)

(* TODO: get rid of Core? *)
(* let parse ch = 
    let data = really_input_string ch (in_channel_length ch) in
    let state = Parlex.Lexer.State.make data in
    match Parlex.Lexer.Iterator.all Lexer.Scanner.next state with
    | Error e -> Error (`LexError e)
    | Ok lexemes ->
        let parser_state = Parlex.Parser.make lexemes in
        match Parser.parse_root_stmts parser_state with
        | Ok stmts -> Ok stmts
        | Error e -> Error (`ParseError e) *)



    (* 
        create lexer context from stream (parlex/readahead?)
        parse file using the given settings
        return ast
        expose Ast.Node
        move Pos/Result to Common ?? 
     *)
