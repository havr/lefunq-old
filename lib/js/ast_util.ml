(* TODO: move staff from Ast to Ast.Node, move it here *)
open Base

module Expr = struct 
    let binary op left right = Ast.Expr.Binary (Ast.Binary.{op; left; right})
    let str value = Ast.Expr.Str (Ast.Str.{value})
    let num value = Ast.Expr.Num (Ast.Num.{value})
    let num_int value = Ast.Expr.Num (Ast.Num.{value = value |> Int.to_string})
    let index value expr = Ast.Expr.Index (Ast.Index.{expr; value})
    let property name expr = Ast.Expr.Property Ast.Property.{expr; name}
    let call args expr = Ast.Expr.Call Ast.Call.{expr; args}
    let parens expr = Ast.Expr.Parens expr
    let lambda params stmts = Ast.Expr.Lambda (Ast.Lambda.make params stmts)
    let ident value = Ast.Expr.Ident (Ast.Ident.make value) 
end

module Stmt = struct 
    let cond conds else_ = Ast.Cond.{
        conds = List.map conds ~f: (fun (if_, then_) -> {if_; then_});
        else_
    }
end

module Lambdas = struct 
    let scoped param_args body =
        let params = List.map param_args ~f: (fun (param, _) -> param) in
        let args = List.map param_args ~f: (fun (_, arg) -> arg) in
        Expr.call args @@ Expr.parens (Expr.lambda params body)
end

module Apply = struct 
    let rec curried fn = function
        | [] -> fn
        | arg :: rest -> 
            let map_arg = match arg with
                (* TODO: keep it as is, remove voids when optimize ast *)
                | Ast.Expr.Void -> []
                | arg -> [arg]
            in
            curried (Ast.Expr.Apply (Ast.Apply.make fn map_arg)) rest
end

module Lambda = struct
    let curried args body = 
        let rec loop = function
            | [] -> body
            | arg :: rest -> 
                let args = if String.is_empty arg then [] else [arg] in
                Ast.Expr.Lambda (Ast.Lambda.make args [Ast.Block.Return (Ast.Return.make (loop rest))])
        in loop args
end

module Const = struct 
    let expr name e= Ast.Block.Const (Ast.Const.{name; expr = Ast.Const.Expr e})
end