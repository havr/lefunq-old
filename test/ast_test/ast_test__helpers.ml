open Common

let test parser to_pp ~input ~expect =
  let lexemes = Ast.Scanner.scan_all input |> Result.get_ok in
  let (result, _) = match Ast.Comb.(parser.fn) (Ast.Comb.State.make lexemes) with
  | Ok r -> r
  | Error e ->
    Alcotest.fail ("unexpected error: "  ^ (Ast.Comb.err_to_string e))
  in
  let got_ast = Pp.to_string [to_pp result] in
  let expect_ast = Pp.to_string [to_pp expect] in
  Alcotest.(check string) "ast match" expect_ast got_ast