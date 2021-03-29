open Typed
open Base
open Helper

module Unify = struct 
  let results expect substs = List.filter_map expect ~f: (fun (var', subst) ->
    match Map.find substs var' with
    | Some t ->
      if not @@ Type.equals t subst then begin
        let expect_str = Type.to_string subst in
        let got_str = Type.to_string t in
        Some (var' ^ ": " ^ got_str ^ " != " ^ expect_str)
      end else None
    | None -> 
      Some (var' ^ ": no substitution")
  )
end

let assert_errors expected got =
  let errors_expected_not_found = difference 
    ~equals: (Typed.Erro.equals) expected got in
  let unexpected_got = difference 
    ~equals: (Typed.Erro.equals) got expected in
  let map_errors errors = errors |> List.map ~f:Typed.Erro.to_string in
  let exp_not_found = if List.length errors_expected_not_found > 0 then
    map_errors errors_expected_not_found 
    |> String.concat ~sep: "\n" else "" in
  let unexp = if List.length unexpected_got > 0 then
    map_errors unexpected_got 
    |> String.concat ~sep: "\n" else "" in
  let labeled_exp = if String.is_empty exp_not_found then
    "" else "expected error: " ^ exp_not_found in
  let labeled_unexp = if String.is_empty unexp then
    "" else "unexpected error: " ^ unexp in

  let issues = [labeled_exp; labeled_unexp] 
  |> List.filter ~f: (fun s -> not @@ String.is_empty s) in
  if List.length issues > 0 then
    issues 
      |> String.concat ~sep: "\n"
      |> Alcotest.fail
      |> ignore

  let check_results ?(errors=[]) ~ctx ~expect_type ~expect subst typ = 
    let typ = Typed.Subst.apply_substs subst typ in
    let results = Unify.results expect subst in
    begin 
      if not @@ Typed.Type.equals typ expect_type then
        Alcotest.fail @@ String.concat [
          "(got) "; Typed.Type.to_string typ; " != (expected) "; Typed.Type.to_string expect_type
        ]
    end;
    begin 
      if List.length results > 0 then
        Alcotest.fail (String.concat ~sep: "\n" results)
    end;
    assert_errors errors Typed.Inferno.(ctx.errors)