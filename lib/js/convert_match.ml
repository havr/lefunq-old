open Base
open Common
open Ast_util
(* TODO: move to Js.Ast *)

let equals_check = Expr.binary "==" 

module Ctx = struct 
    type t = {
        conds: Ast.Expr.t list;
        params: (string * Ast.Expr.t) list
    }
    let empty = {conds = []; params = []}
    let with_cond cond = {conds = [cond]; params = []}
    let with_param name accessor = {conds = []; params = [name, accessor]}

    let merge = function
        | [] -> {conds = []; params = []}
        | ctxs -> 
             Option.value_exn (ctxs 
            |> List.reduce ~f: (fun acc {conds; params} -> {conds = acc.conds @ conds; params = acc.params @ params}))
end

module ArrayUtils = struct 
    let slice ~start ?fin expr = 
        let open Expr in
        let args = [Some (num_int start); Option.map ~f:num_int fin]
            |> List.filter_map  ~f: (fun f -> f) in
        call args (property "slice" expr)
end

let pattern_checks accessor pattern = 
    let rec step accessor pattern =
        match pattern with
        | Typed.Match.Any -> Ctx.empty
        | Typed.Match.Unit -> Ctx.empty
        | Typed.Match.Str s -> Ctx.with_cond (equals_check accessor (Expr.str s)) 
        | Typed.Match.Int i -> Ctx.with_cond (equals_check accessor (Expr.num i)) 
        | Typed.Match.Param p -> Ctx.with_param p.scope_name accessor 
        | Typed.Match.Tuple tup ->
            List.mapi tup ~f: (fun i elem -> 
                let accessor = Expr.index (Expr.num_int i) accessor in
                step accessor elem
            )
            |> Ctx.merge
        | Typed.Match.List li ->
            let acc_check = Ctx.with_cond (equals_check (Expr.property "length" accessor) (Expr.num_int @@ List.length li.items)) in
            let items = List.mapi li.items ~f: (fun i elem -> 
                let accessor = Expr.index (Expr.num_int i) accessor in
                step accessor elem
            ) in
            let rest = match li.rest with
                | None -> Ctx.empty 
                | Some (Typed.Match.Param p) -> Ctx.with_param p.scope_name (ArrayUtils.slice ~start: (List.length li.items) accessor)                | _ -> (raise Common.Unreachable)
            in Ctx.merge @@ [acc_check] @ items @ [rest]
    in let ctx = step accessor pattern in
    let cond = List.map ctx.conds ~f: Expr.parens
    |> List.reduce ~f: (fun acc cond -> Expr.binary "&&" acc cond) in
    cond, ctx.params


let match' ~stmts ~map_return accessor cases =
    let else', cases = Util.Lists.last_rest cases in
    let inject_return params s = 
        let block = List.map params ~f: (fun (scope_name, expr) -> (Ast.Block.Const (Ast.const_expr scope_name expr))) in
        let ret, intern = Util.Lists.last_rest (block @ Typed.Node.Match.(stmts s.stmts)) in
        intern @ [map_return ret]
    in
    let conds' = List.map cases ~f: (fun case ->
        (* TODO: inject if checks *)
        let cond, params = pattern_checks accessor Typed.Node.Match.(case.pattern) in
        (Option.value_exn cond, inject_return params case)
    ) in
        (* the typechecker should assert that the last line executes all the time. 
           so it's useless to check conditions*)
    let else_branch = 
        let _, params = pattern_checks accessor Typed.Node.Match.(else'.pattern) in
        inject_return params else'
    in
    Stmt.cond conds' (Some else_branch)
