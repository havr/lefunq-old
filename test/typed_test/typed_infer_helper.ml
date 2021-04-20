open Typed
open Base
open Common
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
    ~equals: (Typed.Errors.equals) expected got in
  let unexpected_got = difference 
    ~equals: (Typed.Errors.equals) got expected in
  let map_errors errors = errors |> List.map ~f:Typed.Errors.to_string in
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
    let typ = Typed.Subst.apply subst typ in
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

let rec find_toplevel_let path modu_stmts =
    match path, modu_stmts with
    | [], _ -> None
    | name :: [], entries -> 
        (List.find_map entries ~f:(function
        | Typed.Module.Binding t when String.equal t.given_name name -> Some t 
        | _ -> None
        ) |> Option.map ~f: (fun t -> (t, [])))
    | name :: rest, entries -> (
        let rec iter = function 
            | (Typed.Module.Module m) :: _ when String.equal m.given_name name -> 
                find_toplevel_let rest m.entries
            | (Typed.Module.Binding b) :: _ when String.equal b.given_name name -> 
                Some (b, rest)
            | _ :: rest -> iter rest
            | [] -> None
        in iter entries
    )

let find_let_in_stmts path stmts =
    let rec find_in_stmts path stmts = List.find_map ~f: (find_in_stmt path) stmts
    and find_in_stmt path block =
        match (block, path) with
        | Typed.Stmt.Let t, n :: [] -> (match String.equal t.given_name n with
            | true -> Some t
            | false -> (List.find_map t.block.stmts ~f: (find_in_stmt path))
        )
        | Typed.Stmt.Let t, n :: rest -> (match String.equal t.given_name n with
            | true -> (List.find_map t.block.stmts ~f: (find_in_stmt rest))
            | false -> None
        )
        | Typed.Stmt.Let _, [] -> None 
        | Typed.Stmt.Block b, path -> find_in_stmts path (b.stmts)
        | Typed.Stmt.Expr e, path -> find_in_expr path e
    and find_in_arg path = function
        | Typed.Apply.PosArg {expr} -> find_in_expr path expr
        | Typed.Apply.NameArg {expr; _} -> find_in_expr path expr
    and find_in_expr path expr = 
        match expr with
        | Value _ -> None 
        | Ident _ -> None
        | Apply a -> (match find_in_expr path a.fn with
            | Some m -> Some m
            | None -> List.find_map a.args ~f: (find_in_arg path)
        )
        | Lambda lam ->
            find_in_stmts path lam.block.stmts
        | Cond c ->
            (match (List.find_map c.cases ~f: (fun case ->
                match find_in_stmts path case.if_.stmts with
                | Some m -> Some m
                | None -> find_in_stmts path case.then_.stmts
            )) with
            | Some m -> Some m
            | None -> (Option.find_map c.else_ ~f: (fun b -> find_in_stmts path b.stmts)))
        | Li li -> List.find_map li.items ~f:(find_in_expr path)
        | Tuple tup ->  List.find_map tup.exprs ~f:(find_in_expr path)
        | Foreign _ -> None
        | Match m -> (match find_in_expr path m.expr with
            | Some m -> Some m
            | None -> List.find_map m.cases ~f: (fun case -> find_in_stmts path case.stmts)
        )
    in find_in_stmts path stmts

let find_ident name stmts =
    let rec find_in_stmts = List.find_map ~f: (function
        | Typed.Stmt.Let _ -> None 
        | Typed.Stmt.Block b -> find_in_stmts (b.stmts)
        | Typed.Stmt.Expr e -> find_in_expr e
    )
    and find_in_pattern = function
        | Typed.Match.Param p when String.equal p.given_name name -> Some p.typ
        | Typed.Match.Tuple t -> List.find_map t ~f: find_in_pattern
        | Typed.Match.List li -> (match List.find_map li.items ~f: find_in_pattern with
            | Some m -> Some m
            | None -> Option.find_map li.rest ~f: find_in_pattern
        )
        | _ -> None

    and find_in_arg = function
        | Typed.Apply.PosArg {expr} -> find_in_expr expr
        | Typed.Apply.NameArg {expr; _} -> find_in_expr expr

    and find_in_expr = function
        | Value _ -> None 
        | Ident t when String.equal t.resolved.given name -> Some (t.typ)
        | Ident _ -> None
        | Apply a -> (match find_in_expr a.fn with
            | Some m -> Some m
            | None -> List.find_map a.args ~f: find_in_arg
        )
        | Lambda lam ->
            find_in_stmts lam.block.stmts
        | Cond c ->
            (match (List.find_map c.cases ~f: (fun case ->
                match find_in_stmts case.if_.stmts with
                | Some m -> Some m
                | None -> find_in_stmts case.then_.stmts
            )) with
            | Some m -> Some m
            | None -> (Option.find_map c.else_ ~f: (fun b -> find_in_stmts b.stmts)))
        | Li li -> List.find_map li.items ~f:find_in_expr
        | Tuple tup ->  List.find_map tup.exprs ~f:find_in_expr
        | Foreign _ -> None
        | Match m -> (match find_in_expr m.expr with
            | Some m -> Some m
            | None -> List.find_map m.cases ~f: (fun case ->
                match find_in_pattern case.pattern with
                | Some m -> Some m
                | None -> find_in_stmts case.stmts
            )
        )
    in find_in_stmts stmts

let trunc_last list = match List.rev list with
    | [] -> raise (Invalid_argument "list is empty")
    | head :: rest -> head, List.rev rest

let find_let path stmts = 
    (match find_toplevel_let path stmts with
    | None -> None
    | Some (b, []) -> Some b
    | Some (b, rest) -> 
        (match rest with
        | [] -> Some b
        | rest -> find_let_in_stmts rest b.block.stmts)
    )

let split_qualified = String.split ~on: '.'

let assert_let str_path scheme stmts = 
    (match find_let (split_qualified str_path) stmts with
    | None -> Alcotest.fail ("let-binding" ^ str_path ^ " not found")
    | Some t -> 
        Alcotest.(check(module String))
            str_path
            (Type.scheme_to_string scheme) 
            (Type.scheme_to_string (Option.value_exn t.scheme)))
    |> ignore

let assert_ident str_path typ entries =
    let name, rest' = trunc_last (split_qualified str_path) in
    match find_let rest' entries with
    | None -> Alcotest.fail ("let-binding containing " ^ str_path ^ " not found")
    | Some l ->
        match find_ident name Let.(l.block.stmts) with
        | None -> Alcotest.fail ("ident " ^ str_path ^ " not found")
        | Some result_typ -> 
            Alcotest.(check(module String))
                str_path
                (Type.to_string typ) 
                (Type.to_string result_typ)

    (* let path = String.split ~on: '.' str_path in
    let result = (match find_toplevel_let path stmts with
    | None -> None
    | Some (b, []) -> Alcotest.fail (str_path ^ "points to a binding, not ident")
    | Some (b, rest) -> 
        let name, rest' = trunc_last rest in
        (match rest' with
        | [] -> Some b
        | rest' -> find_let_in_stmts rest' b.block.stmts)
            |> Option.find_map ~f:(fun t -> find_ident name Let.(t.block.stmts))
    ) in
    (match result with
    | Some typ -> 
    |> ignore *)

type test_result = Success of {
    asserts: (Module.entry list -> unit) list
} | Failure of {
    errors: (Errors.t list)
}

let test ~code ~expect () = 
    match Ast.of_string ~file:"" code with
    | Error e -> Alcotest.fail (Common.Err.to_string e)
    | Ok ast -> 
        let result = root ~source: "" ~resolve_source: (fun _ -> raise Common.Unexpected) ast in 
        match (expect, result) with
        | (Success {asserts}, Ok ast) -> 
            List.iter asserts ~f: (fun asser -> asser ast.entries)
        | Failure {errors}, Error got_errors ->
            assert_errors errors (List.map ~f:Errors.clear_range got_errors)
        | Success _, Error e -> (Alcotest.fail (e |> List.map ~f:Typed.Errors.to_string |> String.concat ~sep:"\n"))
        | Failure _, Ok ast -> 
            Common.log ["\n"; Pp.to_string [Typed.Node.Print_node.modu ast]];
            Alcotest.fail ("Expected failure, got OK")