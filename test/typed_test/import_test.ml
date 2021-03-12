(* TODO: test proper imports *)

(* module TyNode = Typed.Node

let test_transform_highlevel () = 
  let input = {|import Foo "foo"|} in

  let expect = TyNode.Import.{
      source = Span.empty "foo";
      resolved_source = "";
      names = [
        {name = Span.empty "Foo"; path = []; resolved = None};
      ]
  } in 

  let lexemes = Ast.Scanner.scan_all input |> Result.get_ok in
  let result = Ast.Comb.(Ast.Parser.import.fn) (Ast.Comb.State.make lexemes) in
  let (result, _) = match result with
  | Ok r -> r
  | Error e -> begin print_endline (Ast.Comb.err_to_string e); Alcotest.fail "unexpected error" end
  in
  let transf = Typed.Transform.import result in
  let got_ast = Pp.to_string [TyNode.Import.pretty_print transf] in
  let expect_ast = Pp.to_string [TyNode.Import.pretty_print expect] in
  Alcotest.(check string) "ast match" expect_ast got_ast


let tests = [
    "transform_highlevel", `Quick, test_transform_highlevel
] *)