open Base

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
    '$', "dollar";
    '|', "pipe";
    '>', "more"
]

let operators = operator_mappings |> List.map ~f:(fun (m, _) -> m)

let is_operator str = 
    let ch = String.get str 0 in 
    List.find ~f:(fun n -> Char.equal n ch) operators |> Option.is_some

let map_operator op = List.fold (String.to_list op) ~init: "" ~f: (fun result char -> 
    result ^ "$" ^ (List.find_map_exn operator_mappings ~f: (fun (c, alias) -> 
        if Char.equal c char then Some alias else None ))
)

let ident_value name = 
    if is_operator name then map_operator name else 
        List.fold ["'", "$prime"; "!", "$unsafe"] ~init: name ~f: (fun accum (str, sub) -> 
            String.substr_replace_all accum ~pattern: str ~with_: sub)

let ident n = Ast.Ident.{value = ident_value Typed.Ident.(n.given_name)}

let basic b =
    if phys_equal Typed.Value.(b.type_) Typed.BaseTypes.str 
        then Ast.Expr.Str(Ast.Str.{value = b.value})
    else if phys_equal Typed.Value.(b.type_) Typed.BaseTypes.int 
        then Ast.Expr.Num(Ast.Num.{value = b.value})
    else if phys_equal Typed.Value.(b.type_) Typed.BaseTypes.unit 
        then Ast.Expr.Void
    else raise @@ Unexpected ("unexpected basic type: " ^ (Typed.Type.to_string b.type_) ^ "(has value: " ^ b.value ^ ")")

let rec apply n = begin
    let apply_seq fn = function
    | [] -> raise @@ Unexpected "apply has no arguments"
    | first :: rest ->
        let init = Ast.Expr.Apply(Ast.Apply.{
            fn=fn;
            args=[expr first]
        }) in
        Base.List.fold ~init: init ~f:(fun acc arg ->
            Ast.Expr.Apply(Ast.Apply.{
                fn=acc;
                args=[expr arg]
            })
        ) rest
    in
    match Typed.Apply.(n.fn) with 
    | Typed.Expr.Ident m -> 
        let local_name = m.given_name in
        if not @@ is_js_operator local_name then
            apply_seq (Ast.Expr.Ident (ident m)) n.args
        else begin
            match n.args with
            | [unary] -> Ast.Expr.Unary (Ast.Unary.{op = local_name; expr = expr unary})
            | [left; right] ->
                let wrap_parens node = match node with 
                | Ast.Expr.Binary b -> 
                    if get_precedence (b.op) < get_precedence (local_name) then
                        Ast.Expr.Parens node
                    else node
                | _ -> node
                in
                let left_ast = wrap_parens @@ expr left in
                let right_ast = wrap_parens @@ expr right in
                Ast.Expr.Binary(Ast.Binary.{op = local_name; left = left_ast; right = right_ast})
            | _ -> raise Unreachable
        end
    | _ -> apply_seq (expr n.fn) n.args

end and expr  = function
    | Typed.Expr.Value m -> basic m
    | Typed.Expr.Ident m -> Ast.Expr.Ident (ident m)
    | Typed.Expr.Apply m -> apply m
    | Typed.Expr.Lambda m -> lambda m
    | Typed.Expr.Cond m -> cond m
    | Typed.Expr.Tuple _ -> raise Common.TODO
and cond n = 
    let cond_expr = function 
        | (Typed.Stmt.Expr e) :: [] -> expr e 
        | b -> Block (block b)
    in
    let conds = Typed.Cond.(n.cases) |> List.map ~f:(fun Typed.Cond.{if_; then_} ->
        let if_ = cond_expr if_.stmts in
        let then_ = block then_.stmts in
        Ast.Cond.{if_ = if_; then_ = then_.stmts}
    ) in let cond_block = Ast.Block.Cond Ast.Cond.{
        conds=conds; else_ = n.else_ |> Option.map ~f: (fun m  -> Typed.Block.(block m.stmts).stmts)
    }
    in Ast.Expr.Block(Ast.Block.{stmts=[cond_block]})
and lambda n = 
    let open Base in
    match List.rev @@ Typed.Lambda.(n.params) with
    | [] -> raise (Unexpected "lambda without arguments")
    | first :: rest -> 
        let convert_arg = function
            | Typed.Param.Name n -> [n]
            | Typed.Param.Tuple _ -> raise Common.TODO
            | Typed.Param.Unit -> []
        in
        let inner_lambda = Ast.Lambda.{args = convert_arg first.shape; block = block (n.block.stmts)} in
        Ast.Expr.Lambda (List.fold ~init: inner_lambda ~f: (fun inner arg -> 
            Ast.Lambda.{args = convert_arg arg.shape; block = {stmts = [Ast.Block.Return Ast.Return.{expr=Ast.Expr.Lambda inner}]}}
        ) rest)

and block_stmt = function
    | Typed.Stmt.Expr n -> Ast.Block.Expr (expr n)
    | Typed.Stmt.Block n -> Ast.Block.Block (block n.stmts)
    | Typed.Stmt.Let n -> Ast.Block.Const (let_ n)

and block n =
    let map_block_stmt len idx stmt = 
        if phys_equal idx (len - 1) then 
            match stmt with
            | Typed.Stmt.Expr n -> Ast.Block.Return Ast.Return.{ expr = expr n }
            | _ -> block_stmt stmt
        else block_stmt stmt
    in Ast.Block.{ stmts = List.mapi ~f: (map_block_stmt (List.length n)) (n) }

and let_ n = 
    let const_expr = 
        match Typed.Block.(n.block.stmts) with
        | Typed.Stmt.Expr m :: [] -> Ast.Const.Expr (expr m)
        | _ -> Ast.Const.Block  (block n.block.stmts)
    in Ast.Const.{ name = ident_value n.given_name; expr = const_expr }

    
let root_module root =
    List.map Typed.Node.Module.(root.entries) ~f: (function
        | Typed.Node.Module.Binding b -> let_ b
    ) 