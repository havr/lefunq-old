open Parlex


module MatchersTest = struct 
  let check_outcome (ok, next_state) (got_ok, got_pos) = 
    let open Types.Pos in
    let open Lexer.State in
    Alcotest.(check bool) "result" ok got_ok;
    Alcotest.(check int) "position.idx" got_pos.idx next_state.pos.idx;
    Alcotest.(check int) "position.col" got_pos.col next_state.pos.col;
    Alcotest.(check int) "position.row" got_pos.row next_state.pos.row

  let check str parser outcome = 
    let state = Lexer.State.make str in
    let result  = parser state in
    check_outcome result outcome

  let test_char_ok () =
    check "c" (Parlex.Lexer.Matchers.char "abc") (true, Types.Pos.{idx = 1; col = 2; row = 1})

  let test_char_fail () =
    check "c" (Parlex.Lexer.Matchers.char "def") (false, Types.Pos.{idx = 0; col = 1; row = 1})

  let tests = [
    "char ok", `Quick, test_char_ok;
    "char fail", `Quick, test_char_fail;
  ]
end

module LetTest = struct 
(* TODO: utils? *)
  let with_fake_pos a = List.map (fun i -> Parser.{pos = Parlex.Types.Pos.empty; value = i}) a
  open Ast.Lexer.Lexeme
  let test_let () =
    let state = Parser.{
      lexemes = (with_fake_pos [Let; Ident "foo"; Eq; Int "10"]);
      error = None
    } in
    match Ast.Parser.let_.fn state with
      | Ok (node, _) -> 
        Alcotest.(check string) "ident" node.let_ident.ident "foo";
        begin
          match node.expr with
          | `Int {int_pos = _; int = "10"} -> ()
          | _ -> Alcotest.fail "omg"
        end
      | Error e -> Alcotest.fail (Parlex.Parser.print_err e)

  (* let test_parser_ok () = 
    let state = Parser.{lexemes = ["hello"; "world"]; error = None} in
    let foo = (Parser.lexeme (fun lexeme -> lexeme == "hello")).fn state in
    match foo with
    | Error err -> Alcotest.fail ("unexpected error: " ^ (Parser.print_err err)) |> ignore
    | Ok (result, _) -> Alcotest.(check string) "parses correctly" "hello" result |> ignore *)

  (*let (<*>) a fn = *)
  (* TODO: what to do with this crap *)
  (* let apply fn p = p >>= (fun a -> return @@ fn a) in *)

  (* let test_parser_monad () = 
    let state = Parser.{lexemes = ["("; "hello"; ","; "world"; ")"]; error = None} in
    let open Parser.Monad.Syntax in
    let rec separatedBy sep parser =
      let* p = parser in
      let* sepp = Parser.Monad.maybe sep in
      match sepp with
      | Some _ -> Parser.Monad.return (p :: (separatedBy sep parser))
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
      Parser.Monad.return (hell |> String.concat ",")

    in let foo = combined.fn state in

    match foo with
    | Error err -> Alcotest.fail ("unexpected error: " ^ (Parser.print_err err)) |> ignore
    | Ok (result, _) -> Alcotest.(check string) "parses correctly" "hello" result |> ignore *)

  let tests = [
    "let int", `Quick, test_let;
    (* "lexeme ok", `Quick, test_parser_monad; *)
  ]
end

let () = Alcotest.run "Parlex" [
  "Matchers", MatchersTest.tests;
  "Let", LetTest.tests;
  (* "Parser", ParserTest.tests *)
]

