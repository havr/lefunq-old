open Base
open Convert_common

let require_nodes ~foreign_bindings ~source root = 
    let open Typed.Node in 
    let x = ref 0 in
    let convert_var_name _ = 
        let name = "__" ^ (Int.to_string !x) in
        x := !x + 1;
        name
    in
    let collect_deps root = 
        let order = ref [] in
        let dups = ref (Set.empty(module String)) in
        let add_dep ?name dep = 
            let skip = (Set.mem !dups dep) || (String.is_empty dep) || (String.equal dep source) in
            if not skip then (
                let n = match name with 
                    | Some n -> n
                    | None -> convert_var_name dep
                in
                order := (dep, n) :: !order; 
                dups := Set.add !dups dep
            ) in
        let using u = (match Using.(u.root) with 
                | Using.Source {resolved; _} -> add_dep resolved; 
                | _ -> ()) 
        in
        let rec block b = block_stmts Block.(b.stmts) 
        and block_stmts = List.iter ~f:(function
            | Stmt.Expr e -> expr e 
            | Stmt.Block bs -> block bs
            | Stmt.Let t -> block t.block
            | Stmt.Using u -> using u
        )

        and expr = function
            | Value _ -> ()
            | Li li -> List.iter li.items ~f:(function 
                | Li.Single e -> expr e 
                | Li.Spread e -> expr e
            )
            | Foreign _ ->
                (* TODO: check it earlier? *)
                add_dep ~name: foreign_require (Option.value_exn foreign_bindings)
            | Ident id -> 
                (match id.qual.name.resolved with
                    | None -> Common.log["Warning: unresolved identifier:"; id.qual.name.given];
                    | Some n -> add_dep n.source
                )
            | Apply app -> 
                expr app.fn;
                List.iter app.args ~f: (function 
                    | PosArg{expr = e} -> expr e
                    | NameArg{expr = e; _} -> expr e
                )
            | Lambda lam -> block lam.block
            | Match m -> (* TODO: visit pattern *)
                (expr m.expr);
                List.iter m.cases ~f: (fun case -> block_stmts case.stmts)
            | Cond t -> 
                List.iter t.cases ~f: (fun {if_; then_} ->
                    block if_;
                    block then_;
                );
                Option.iter t.else_ ~f:block
            | Tuple t -> List.iter t.exprs ~f:expr
        in
        let rec modu m = 
            List.iter Module.(m.entries) ~f:(function
            | Module.Binding b -> block b.block
            | Module.Using im -> using im
            | Module.Module m -> modu m
            | Module.Typedef _ -> () (* TODO: process typedef deps *)
        ) 
        in modu root; !order
    in 
    let deps = collect_deps root in
    let call name args = Ast.Apply.{
        fn = Ast.Expr.Ident (Ast.Ident.{value=name});
        args = args;
    } in
    let str value = Ast.Str.{value} in
    let require name = call "require" [Ast.Expr.Str (str name)] in
    let nodes = List.map deps ~f: (fun (name, var_name) -> 
        Ast.Block.Const (Ast.Const.expr var_name (Ast.Expr.Apply (require name)))) in
    (nodes, Map.of_alist_exn(module String) deps)