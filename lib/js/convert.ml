open Base

let foreign_require = "__foreign" (* TODO: proper names *)

let js_operators = ["+"; "-"; "*"; "/"; ">"; "<"; "=="; "!="; "%"]

let is_js_operator ident = List.exists ~f:((String.equal) ident) js_operators

exception Unreachable

exception Unexpected of string

(* TODO: move into common/utils *)
let find_idx fn list = 
    let rec find_idx' n = function
        | [] -> None
        | x :: rest -> if fn x then Some n else find_idx' (n + 1) rest
    in find_idx' 0 list

let binary_op_precedence = [["+"; "-"]; ["*"; "/"; "%"]; [">"; "<"; "<="; ">="; "=="]]
let get_precedence op = 
    find_idx (fun list -> List.find ~f:(fun v -> String.equal v op) list |> Option.is_some) (binary_op_precedence) 
    |> Option.value ~default:(-1)

(* TOOD: module operator-related stuff into a submodule *)
let operator_mappings = [
    '$', "$dollar";
    '|', "$pipe";
    '>', "$more"
]

let operators = operator_mappings |> List.map ~f:(fun (m, _) -> m)

let is_operator str = 
    let ch = String.get str 0 in 
    List.find ~f:(fun n -> Char.equal n ch) operators |> Option.is_some

let map_operator op = List.fold (String.to_list op) ~init: "" ~f: (fun result char -> 
    let mapped = List.find_map operator_mappings ~f: (fun (c, alias) -> 
        if Char.equal c char then Some alias else None ) in
    result ^ (Option.value ~default: (Char.to_string char) mapped)
) 

let ident_value name = 
    if is_operator name then map_operator name else 
        List.fold ["'", "$prime"; "!", "$unsafe"] ~init: name ~f: (fun accum (str, sub) -> 
            String.substr_replace_all accum ~pattern: str ~with_: sub)

module Dsl = struct 
    let apply fn args = Ast.Expr.Apply Ast.Apply.{fn; args}

end

open Common

type ctx = {
    source: string;
    required_sources: string StringMap.t
}

let ident ~ctx n = 
    let id = Option.value_exn Typed.Ident.(n.resolved) in
    (*TODO: epic kludge. introduce foreigns asap  *)
    if String.equal id.name "println" then begin 
        Ast.Ident.{value = ident_value "println"}
    end
    else
    let value = match id.source with
    | "" -> ident_value id.name
    | source -> 
        if String.equal ctx.source source then 
            ident_value id.name
        else
            match Map.find ctx.required_sources source with
            | Some obj -> ident_value (obj ^ "." ^ id.name)
            | None -> raise @@ Invalid_argument ("Source not found: " ^ (Typed.Symbol.Id.to_string id))
    in Ast.Ident.{value}

let basic b =
    if phys_equal Typed.Value.(b.type_) Typed.Base_types.str 
        then Ast.Expr.Str(Ast.Str.{value = b.value})
    else if phys_equal Typed.Value.(b.type_) Typed.Base_types.int 
        then Ast.Expr.Num(Ast.Num.{value = b.value})
    else if phys_equal Typed.Value.(b.type_) Typed.Base_types.unit 
        then Ast.Expr.Void
    else raise @@ Invalid_argument ("unexpected basic type: " ^ (Typed.Type.to_string b.type_) ^ "(has value: " ^ b.value ^ ")")


let rec apply ~ctx n = begin
    let apply_seq ~ctx fn = function
    | [] -> raise @@ Invalid_argument "apply has no arguments"
    | first :: rest ->
        let init = Dsl.apply fn [expr ~ctx first] in
        Base.List.fold rest ~init: init ~f:(fun acc arg -> Dsl.apply acc [expr ~ctx arg]) 
    in
    match Typed.Apply.(n.fn) with 
    | Typed.Expr.Ident m -> 
        (* TODO: !!FQN like!! import.value *)
        let local_name = (Option.value_exn m.resolved).name in
        if not @@ is_js_operator local_name then
            apply_seq ~ctx (Ast.Expr.Ident (ident ~ctx m)) n.args
        else begin
            match n.args with
            | [unary] -> Ast.Expr.Unary (Ast.Unary.{op = local_name; expr = expr ~ctx unary})
            | [left; right] ->
                let wrap_parens node = match node with 
                | Ast.Expr.Binary b -> 
                    if get_precedence (b.op) < get_precedence (local_name) then
                        Ast.Expr.Parens node
                    else node
                | _ -> node
                in
                let left_ast = wrap_parens @@ expr ~ctx left in
                let right_ast = wrap_parens @@ expr ~ctx right in
                Ast.Expr.Binary(Ast.Binary.{op = local_name; left = left_ast; right = right_ast})
            | _ -> raise Unreachable
        end
    | _ -> apply_seq ~ctx (expr ~ctx n.fn) n.args

end and expr ~ctx  = function
    | Typed.Expr.Li li -> Ast.Expr.Li (List.map li.items ~f: (expr ~ctx))
    | Typed.Expr.Value m -> basic m
    | Typed.Expr.Ident m -> Ast.Expr.Ident (ident ~ctx m)
    | Typed.Expr.Apply m -> apply ~ctx m
    | Typed.Expr.Lambda m -> lambda ~ctx m
    | Typed.Expr.Cond m -> cond ~ctx m
    | Typed.Expr.Tuple t -> Ast.Expr.Li (List.map t.exprs ~f: (expr ~ctx))
    | Typed.Expr.Foreign f -> Ast.Expr.Ident Ast.Ident.{value = foreign_require ^ "." ^ f.name}
and cond ~ctx n = 
    let cond_expr = function 
        | (Typed.Stmt.Expr e) :: [] -> expr ~ctx e 
        | b -> Block (block ~ctx b)
    in
    let conds = Typed.Cond.(n.cases) |> List.map ~f:(fun Typed.Cond.{if_; then_} ->
        let if_ = cond_expr if_.stmts in
        let then_ = block ~ctx then_.stmts in
        Ast.Cond.{if_ = if_; then_ = then_.stmts}
    ) in let cond_block = Ast.Block.Cond Ast.Cond.{
        conds=conds; else_ = n.else_ |> Option.map ~f: (fun m  -> Typed.Block.(block ~ctx m.stmts).stmts)
    }
    in Ast.Expr.Block(Ast.Block.{stmts=[cond_block]})

and lambda_like ~ctx params bl =  
    let open Base in
    match List.rev @@ params with
    | [] -> raise (Invalid_argument "lambda without arguments")
    | first :: rest -> 
        let convert_arg = function
            | Typed.Param.Name n -> [n.resolved]
            | Typed.Param.Tuple _ -> raise Common.TODO
            | Typed.Param.Unit -> []
        in
        let inner_lambda = Ast.Lambda.{args = convert_arg Typed.Param.(first.shape); block = block ~ctx Typed.Block.(bl.stmts)} in
        Ast.Expr.Lambda (List.fold ~init: inner_lambda ~f: (fun inner arg -> 
            Ast.Lambda.{args = convert_arg arg.shape; block = {stmts = [Ast.Block.Return Ast.Return.{expr=Ast.Expr.Lambda inner}]}}
        ) rest)
and lambda ~ctx n = lambda_like ~ctx Typed.Lambda.(n.params) n.block

and block_stmt ~ctx = function
    | Typed.Stmt.Expr n -> Ast.Block.Expr (expr ~ctx n)
    | Typed.Stmt.Block n -> Ast.Block.Block (block ~ctx n.stmts)
    | Typed.Stmt.Let n -> Ast.Block.Const (let_ ~ctx n)

and block ~ctx n =
    let map_block_stmt len idx stmt = 
        if phys_equal idx (len - 1) then 
            match stmt with
            | Typed.Stmt.Expr n -> Ast.Block.Return Ast.Return.{ expr = expr ~ctx n }
            | _ -> block_stmt ~ctx stmt
        else block_stmt ~ctx stmt
    in Ast.Block.{ stmts = List.mapi ~f: (map_block_stmt (List.length n)) (n) }

and let_ ~ctx n = 
    let const_expr = 
        match Typed.Let.(n.params) with
        | [] ->
            (match Typed.Block.(n.block.stmts) with
            | Typed.Stmt.Expr m :: [] -> Ast.Const.Expr (expr ~ctx m)
            | _ -> Ast.Const.Block  (block ~ctx n.block.stmts))
        | params -> Ast.Const.Expr (lambda_like ~ctx params n.block)
    (* for nested modules we truncate module path*)
    in Common.log[n.scope_name; ident_value n.scope_name; map_operator n.scope_name]; Ast.Const.{ name = ident_value n.scope_name; expr = const_expr }


let root_module root =
    List.map Typed.Node.Module.(root.entries) ~f: (function
        | Typed.Node.Module.Binding b -> let_ b
        | _ -> raise Common.TODO
    ) 