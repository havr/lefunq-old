module Lexeme = struct
  type t = string
  let to_string t = t
  let eof = "eof"
end

open Parlex

module Parser = Parlex.Parser(Lexeme)

open Parser
open Parser.Syntax

let make_fake strs = 
  (* let open Parlex.Lexer in *)
  List.map (fun str -> {value = str; start_pos = Pos.empty; end_pos = Pos.empty}) strs
  |> Parser.State.make

let parser = begin
  let+ first = one_value (fun a -> if a == "hello" then Some("hello") else None) 
  and+ second = one_value (fun a -> if a == "world" then Some("world") else None) in
  first.value ^ second.value
end

let one_ok () = 
  let state = make_fake ["hello"; "world"] in
  let result = parser.fn state in match result with
  | Ok (value, _) -> Alcotest.(check string) "matches" "helloworld" value
  | Error _ -> Alcotest.fail "Unexpected error" 

let one_err () = 
  let state = make_fake ["hello"; "desu"] in
  match parser.fn state with 
  | Ok _ -> Alcotest.fail "Unexpected ok" 
  (* TODO: proper error checking *)
  | Error _ -> Alcotest.(check string) "matches" "1" "1"
  
let one_eof () = 
  let state = make_fake ["hello"] in
  (* TODO: proper error checking *)
  match parser.fn state with 
  | Ok (lexeme, _) ->
    Alcotest.fail ("Unexpected match: " ^ lexeme)
  | Error e -> 
    Alcotest.(check string) "matches" "unexpected lexeme: eof" e.err_msg

let tests = [
  "one_ok", `Quick, one_ok;
  "one_err", `Quick, one_err;
  "one_eof", `Quick, one_eof;
]