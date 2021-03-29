open Base

(* TODO: move to Js.Ast *)
module Expr = struct 
    let binary op left right = Ast.Expr.Binary (Ast.Binary.{op; left; right})
    let str value = Ast.Expr.Str (Ast.Str.{value})
    let num value = Ast.Expr.Num (Ast.Num.{value})
    let num_int value = Ast.Expr.Num (Ast.Num.{value = value |> Int.to_string})
    let index value expr = Ast.Expr.Index (Ast.Index.{expr; value})
    let property name expr = Ast.Expr.Property Ast.Property.{expr; name}
    let call args expr = Ast.Expr.Call Ast.Call.{expr; args}
    let parens expr = Ast.Expr.Parens expr
end
module Stmt = struct 
    let cond conds else_ = Ast.Cond.{
        conds = List.map conds ~f: (fun (if_, then_) -> {if_; then_});
        else_
    }
end

let equals_check = Expr.binary "==" 

type case_ctx = {
    conds: Ast.Expr.t list;
    params: (string * Ast.Expr.t) list
}

module ArrayUtils = struct 
    let slice ~start ?fin expr = 
        let open Expr in
        let args = [Some (num_int start); Option.map ~f:num_int fin]
            |> List.filter_map  ~f: (fun f -> f) in
        call args (property "slice" expr)
end

let pattern_checks accessor pattern = 
    let merge = function
        | [] -> {conds = []; params = []}
        | ctxs -> 
             Option.value_exn (ctxs 
            |> List.reduce ~f: (fun acc {conds; params} -> {conds = acc.conds @ conds; params = acc.params @ params}))
    in

    let rec step accessor pattern =
        match pattern with
        | Typed.Match.Str s -> {conds = [equals_check accessor (Expr.str s)]; params = []}
        | Typed.Match.Int i -> {conds = [equals_check accessor (Expr.num i)]; params = []}
        | Typed.Match.Param p -> {conds = []; params = [p.scope_name, accessor]}
        | Typed.Match.Tuple tup ->
            List.mapi tup ~f: (fun i elem -> 
                let accessor = Expr.index (Expr.num_int i) accessor in
                step accessor elem
            )
            |> merge
        | Typed.Match.List li ->
            let acc_check = {conds = [equals_check (Expr.property "length" accessor) (Expr.num_int @@ List.length li.items)]; params = []} in
            let items = List.mapi li.items ~f: (fun i elem -> 
                let accessor = Expr.index (Expr.num_int i) accessor in
                step accessor elem
            ) in
            let rest = match li.rest with
                | None -> {conds = []; params = []}
                | Some (Typed.Match.Param p) -> {
                    conds = []; 
                    params = [
                        p.scope_name,
                        ArrayUtils.slice ~start: (List.length li.items) accessor 
                    ]
                }
                | _ -> (raise Common.Unreachable)
            in merge @@ [acc_check] @ items @ [rest]
    in let ctx = step accessor pattern in
    let cond = Option.value_exn (List.map ctx.conds ~f: Expr.parens
    |> List.reduce ~f: (fun acc cond -> Expr.binary "&&" acc cond))
    |> Expr.parens in
    cond, ctx.params


let param_block = List.map ~f: (fun (scope_name, expr) ->
    Ast.Block.Assign (Ast.Assign.{ident = scope_name; expr})
)

let match' ~stmts ~map_return accessor cases =
    let else', cases = Util.Lists.last_rest Node.Match.(m.cases) in
    let inject_return s = 
        let intern, ret = Util.Lists.last_rest s in
        stmts @@ intern @ [map_return ret]
    in
    let conds' = List.map cases ~f: (fun case ->
        (* TODO: inject if checks *)
        let cond, params = pattern_checks accessor case.pattern in
        let block = param_block params in
        inject_return @@ (param_block params) @ case.stmts
    ) in
        (* the typechecker should assert that the last line executes all the time. 
           so it's useless to check conditions*)
    let else_branch = inject_return case.stmts
    in Stmt.cond conds' else_branch

    

(* let convert_match m ~exprs = 
    List.map Node.Match.(m.cases) ~f: (

    ) *)
    