open Common
open Typed

let ident ident = Ast.{ident_value = ident.local_name}
(*let convert_int int = Ast.{num_value = int.value.int_value}

let convert_str str = Ast.{str_value = Typed.(str.value.str_value) } *)
let js_operators = ["+"; "-"; "*"; "/"; ">"; "<"]
let is_js_operator ident = List.exists ((=) ident) js_operators

exception Unreachable

exception Unexpected of string

let basic basic = 
    if basic.basic_type = Const.BasicTypes.str then 
        Ast.Str Ast.{str_value = Typed.(basic.basic_value)}
    else if basic.basic_type = Const.BasicTypes.int then 
        Ast.Num Ast.{num_value = Typed.(basic.basic_value)}
    else
        raise @@ Unexpected ("unknown basic type: " ^ basic.basic_type ^ "(has value: " ^ basic.basic_value ^ ")")

let rec apply n = begin
    let apply_seq fn = function
    | [] -> raise @@ Unexpected "apply has no arguments"
    | first :: rest ->
        let init = Ast.Apply (Ast.{
            apply_fn=fn;
            apply_args=[expr first]
        }) in
        Base.List.fold ~init: init ~f:(fun acc arg ->
            Ast.Apply (Ast.{
                apply_fn=acc;
                apply_args=[expr arg]
            })
        ) rest
    in
    match Elem.(n.apply_fn) with 
    | Elem.IdentExpr m -> 
        let local_name = m.local_name in
        if is_js_operator local_name then
            match n.apply_args with
            | [unary] -> Ast.Unary (Ast.{unary_op = local_name; unary = expr unary})
            | [left; right] ->
                print_endline local_name;
                Ast.Binary (Ast.{binary_op = local_name; binary_left = expr left; binary_right = expr right})
            | _ -> raise Unreachable
        else apply_seq (Ast.Ident (ident m)) n.apply_args
    | _ ->
        apply_seq (expr n.apply_fn) n.apply_args

end and expr  = function
    | BasicExpr m -> basic m
    | IdentExpr m -> Ident (ident m)
    | ApplyExpr m -> apply m
    | LambdaExpr m -> lambda m
    | CondExpr m -> cond m
and cond n = 
    let cond_expr = match n.cond_block.block_stmts with
    | single_stmt :: [] -> begin 
        match single_stmt with
        | Elem.ExprStmt e -> expr e
        | _ -> Ast.Block (block Elem.(n.cond_block))
    end
    | _ -> Ast.Block (block n.cond_block)
    in
    let cond_then = block Elem.(n.cond_then) in
    let cond_else = n.cond_else |> Option.map block in
        Block Ast.{ block_stmts=[ CondStmt Ast.{ cond_expr = cond_expr; cond_then = cond_then; cond_else = cond_else} ] }
and lambda n = 
    let open Base in
    match n.lambda_args |> List.rev with
    | [] -> raise (Unexpected "lambda without arguments")
    | first :: rest -> 
        let convert_arg = function
            | "_" -> []
            | a -> [a]
        in
        let inner_lambda = Ast.Lambda({lambda_args = convert_arg first.arg; lambda_block = block (n.lambda_block)}) in
        List.fold ~init: inner_lambda ~f: (fun inner arg -> 
            Ast.Lambda({lambda_args = convert_arg arg.arg; lambda_block = {block_stmts = [Ast.ReturnStmt {return_expr=inner}]}})
        ) rest

and block_stmt = function
    | Elem.ExprStmt n -> Ast.ExprStmt (expr n)
    | Elem.BlockStmt n -> Ast.BlockStmt (block n)
    | Elem.LetStmt n -> Ast.ConstStmt (let_ n)

and block n =
    let map_block_stmt len idx stmt = 
        if idx == len - 1 then 
            match stmt with
            | Elem.ExprStmt n -> Ast.ReturnStmt { return_expr = expr n }
            | _ -> block_stmt stmt
        else block_stmt stmt
    in Ast.{ block_stmts = List.mapi (map_block_stmt (List.length n.block_stmts)) Elem.(n.block_stmts) }

and let_ n = 
    let const_expr = 
        match n.let_block.block_stmts with
        | [stmt] -> begin 
            match stmt with
            | Elem.ExprStmt m -> Ast.ConstExpr (expr m)
            | _ -> Ast.ConstBlock (block n.let_block)
        end
        | _ -> Ast.ConstBlock (block n.let_block)
    in Ast.{ name = Elem.(n.let_ident.local_name); const_expr = const_expr }

    