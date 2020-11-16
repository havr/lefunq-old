open Parlex

module Lexeme = struct
  type t = string
  let to_string t = t
end


let check_outcome (ok, next_state) (got_ok, got_pos) = 
  let open Parlex.Pos in
  let open Parlex.Lexer.State in
  Alcotest.(check bool) "result" ok got_ok;
  Alcotest.(check int) "position.idx" got_pos.idx next_state.pos.idx;
  Alcotest.(check int) "position.col" got_pos.col next_state.pos.col;
  Alcotest.(check int) "position.row" got_pos.row next_state.pos.row

let check str parser outcome = 
  let state = Lexer.State.make str in
  let result = parser state in
  check_outcome result outcome

let check_add str parser expect =
  let state = Lexer.State.make str in
  let (matched, state') = parser state in
  let lex = Lexer.State.cut_lex state state' in
  match (expect, matched) with
  | (Some value, true) -> Alcotest.(check string) "result" value lex
  | (Some _, false) -> Alcotest.fail "no match"
  | (None, true) -> Alcotest.fail ("unexpected " ^ lex)
  | (None, false) -> ()

    
let test_char_ok () =
  check "c" (Parlex.Lexer.Matcher.char "abc") (true, Parlex.Pos.{idx = 1; col = 2; row = 1})

let test_char_fail () =
  check "c" (Parlex.Lexer.Matcher.char "def") (false, Parlex.Pos.{idx = 0; col = 1; row = 1})

module Not = struct 
  open Parlex.Lexer.Matcher

    let ok () =
        check_add "a" (not (char "c")) (Some "a")

    let not_ok () =
        check_add "c" (not (char "c")) None

    let tests = [
        "not:ok", `Quick, ok;
        "not:fail", `Quick, not_ok;
    ]
end

let tests = List.concat [Not.tests; [
  "char ok", `Quick, test_char_ok;
  "char fail", `Quick, test_char_fail;
]]

(*
module ParserTest = struct 
  let with_fake_pos a = List.map (fun i -> Parser.{pos = Parlex.Types.Pos.empty; value = i}) a

  let test_parser_ok () = 
    let state = Parser.{lexemes = with_fake_pos ["hello"; "world"]; error = None} in
    let foo = (Parser.lexeme (fun lexeme -> lexeme == "hello")).fn state in
    match foo with
    | Error err -> Alcotest.fail ("unexpected error: " ^ (Parser.print_err err)) |> ignore
    | Ok (result, _) -> Alcotest.(check string) "parses correctly" "hello" result.value |> ignore

  (*let (<*>) a fn = *)


  let test_parser_monad () = 
    let state = Parser.{lexemes = with_fake_pos ["("; "hello"; ","; "world"; ")"]; error = None} in
    let open Parser.Monad in
    let open Parser.Monad.Syntax in

    (* TODO: what to do with this crap *)
    let apply fn p = p >>= (fun a -> return @@ fn a) in

    let rec separatedBy sep parser =
      let* p = parser in
      let* sepp = Parser.Monad.maybe sep in
      match sepp with
      | Some _ -> apply (fun a -> p :: a) (separatedBy sep parser)
      | None -> Parser.Monad.return [p]
    in

    let enclosedBy start end_ parser = 
      let* _ = start in
      let* p = parser in
      let* _ = end_ in 
      Parser.Monad.return p
    in
    let anylex = Parser.lexeme (fun _ -> true) in
    let strlex s = Parser.lexeme (fun lexeme -> lexeme == s) in

    let combined =
      let* hell = enclosedBy (strlex "(") (strlex ")") (separatedBy (strlex ",") anylex) in
      Parser.Monad.return (hell |> String.concat " ")

    in let foo = combined.fn state in

    match foo with
    | Error err -> Alcotest.fail ("unexpected error: " ^ (Parser.print_err err)) |> ignore
    | Ok (result, _) -> Alcotest.(check string) "parses correctly" "hello world" result |> ignore

  let tests = [
    "lexeme ok", `Quick, test_parser_ok;
    "lexeme ok", `Quick, test_parser_monad;
  ]
end

let () = Alcotest.run "Parlex" [
  "Matchers", MatchersTest.tests;
  "Parser", ParserTest.tests
]*)

