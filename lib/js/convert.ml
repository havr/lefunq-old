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
    (match str with 
        | "" -> false
        | str -> let ch = String.get str 0 in 
            List.find ~f:(fun n -> Char.equal n ch) operators |> Option.is_some
    )

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


let resolve_ident ~ctx = function
    | [] -> raise (Common.Unreachable)
    | head :: rest -> 
        let abs = Option.value_exn Typed.Symbol.Resolved.(head.absolute) in
        let head_part = (match abs.source with
            | "" -> ident_value abs.name
            | source -> (match String.equal source ctx.source with
                | true -> ident_value abs.name
                | false -> 
                    ((Map.find_exn ctx.required_sources source) ^ "." ^ (ident_value head.given))
            )
        ) in
        (head_part :: (List.map rest ~f: (fun r -> ident_value r.given)) 
            |> String.concat ~sep:".")

let ident ~ctx n = 
    let id = Option.value_exn ~message: Typed.Ident.("No resolution for:" ^ n.resolved.given) Typed.Ident.(n.resolved.absolute) in
    (*TODO: epic kludge. introduce foreigns asap  *)
    if String.equal id.name "println" then begin 
        Ast.Ident.{value = ident_value "println"}
    end
    else
    Ast.Ident.{value = (resolve_ident ~ctx @@ n.resolution @ [n.resolved])}

let basic b =
    if Typed.Type.equals Typed.Value.(b.typ) Typed.Base_types.str 
        then Ast.Expr.Str(Ast.Str.{value = b.value})
    else if Typed.Type.equals Typed.Value.(b.typ) Typed.Base_types.int 
        then Ast.Expr.Num(Ast.Num.{value = b.value})
    else if Typed.Type.equals Typed.Value.(b.typ) Typed.Base_types.unit 
        then Ast.Expr.Void
    else raise @@ Invalid_argument ("unexpected basic type: " ^ (Typed.Type.to_string b.typ) ^ "(has value: " ^ b.value ^ ")")


let rec apply ~ctx n = begin
    match Typed.Apply.(n.fn) with 
    | Typed.Expr.Ident m -> 
        (* TODO: !!FQN like!! import.value *)
        let local_name = (Option.value_exn m.resolved.absolute).name in
        if not @@ is_js_operator local_name then
            Convert_apply.convert ~expr: (expr ~ctx) n (Ast.Expr.Ident (ident ~ctx m))
        else begin
            match n.args with
            | [PosArg unary] -> Ast.Expr.Unary (Ast.Unary.{op = local_name; expr = expr ~ctx unary.expr})
            | [PosArg left; PosArg right] ->
                let wrap_parens node = match node with 
                | Ast.Expr.Binary b -> 
                    if get_precedence (b.op) < get_precedence (local_name) then
                        Ast.Expr.Parens node
                    else node
                | _ -> node
                in
                let left_ast = wrap_parens @@ expr ~ctx left.expr in
                let right_ast = wrap_parens @@ expr ~ctx right.expr in
                Ast.Expr.Binary(Ast.Binary.{op = local_name; left = left_ast; right = right_ast})
            | _ -> raise Unreachable
        end
    | fn -> 
        Convert_apply.convert ~expr: (expr ~ctx) n (expr ~ctx fn)
        (* input type Int -> Int -> Int
        (* each arg swallows the signature *)
        (* if the order *)
        (* get initial signature *)
        f a b c d = (a) => (b) => (c) => d


        in let conv' = Typed.Node.Expr.typ n.fn in
            List.fold m.args ~init: conv' ~f: (fun covg -> function
                | Typed.Node.Apply.PosArg p ->

                | Typed.Node.Apply.NameArg p ->
            )
        let wrap_sig = 
            |> List.map ~f: (function 
                | PosArg {expr} ->
                | 
            )


        let fn = expr ~ctx n.fn in 
        let loop lam = function
            | Typed.Node.Apply.PosArg {expr = e} -> 
                expr ~ctx e
            | Typed.Node.Apply.NameArg {expr = e} -> 
                let (typ, rest) = Typed.Type.Lambda.split_head lam in
        in
        apply_seq ~ctx (expr ~ctx n.fn) n.args *)

end and expr ~ctx  = function
    | Typed.Expr.Li li -> Ast.Expr.Li (List.map li.items ~f: (expr ~ctx))
    | Typed.Expr.Value m -> basic m
    | Typed.Expr.Ident m -> Ast.Expr.Ident (ident ~ctx m)
    | Typed.Expr.Apply m -> apply ~ctx m
    | Typed.Expr.Lambda m -> lambda ~ctx m
    | Typed.Expr.Match m -> 
        let stmt_to_expr = function
        | Ast.Block.Expr expr -> expr
        | stmt -> Ast.Expr.Block (Ast.Block.{stmts=[stmt]})
        in
        let cond = Convert_match.match' 
            ~stmts: (fun stmts -> List.map stmts ~f: (block_stmt ~ctx))
            ~map_return: (fun stmt -> Ast.Block.Return (Ast.Return.{expr = stmt_to_expr stmt}))
            (Ast_util.Expr.ident "__TODO")
            m.cases
        in
        Ast_util.Lambdas.scoped ["__TODO", expr ~ctx m.expr] [Ast.Block.Cond cond]
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
    let tv = Convert_util.tempvar () in
    let optional name default = 
        let v = tv() in
        let present, default = (match default with
        | None -> (raise Common.TODO)
            (* const a = (temp_param != null) ? Some(temp_param) : None; *)
        | Some value -> 
            (Ast.Expr.Ident (Ast.Ident.make v), expr ~ctx value)
            (* const a = (temp_param != null) ? temp_param : default_value *)
        ) in
        let null_check = Ast.Expr.Binary (Ast.Binary.make "==" (Ast.Expr.Ident (Ast.Ident.make v)) (Ast.Expr.Ident (Ast.Ident.make "null"))) in
        let cond = Ast.Expr.Ternary (Ast.Ternary.make null_check default present) in
        let const = Ast.Block.Const (Ast.Const.expr name cond) in
        (const, v)
    in
    match params with
    | [] -> raise (Invalid_argument "lambda without arguments")
    | params -> 
        (* lambda params -> signature, defaults *)
        let rec destruct_tuple acc pos param = 
            let curr_acc = Ast.Expr.Index (Ast.Index.make acc (Ast.Expr.Num (Ast.Num.from_int pos))) in
            match param with
            | Typed.Param.Name n -> 
                [n.scope, curr_acc]
            | Typed.Param.Tuple tup -> 
                List.mapi tup ~f: (destruct_tuple curr_acc)
                |> Util.Lists.flatten
            | Typed.Param.Unit -> []
        in
        let destruct, params = List.fold_map params ~init: [] ~f: (fun destruct param -> match Typed.Param.(param.value) with 
            | Typed.Param.Positional {pattern = Typed.Param.Unit } ->
                (destruct, "")
            | Typed.Param.Positional {pattern = Typed.Param.Name n} -> 
                (destruct, n.scope)
            | Typed.Param.Positional {pattern = Typed.Param.Tuple tup} -> 
                let acc = tv () in
                let id = Ast.Expr.Ident (Ast.Ident.make acc) in
                let destruct = List.mapi tup ~f: (destruct_tuple id)
                |> Util.Lists.flatten
                |> List.map ~f:(fun (name, value) ->
                    Ast.Block.Const (Ast.Const.make name (Ast.Const.Expr (value)))
                ) in
                (destruct, acc)
            | Typed.Param.Named {name} -> destruct, name.scope
            | Typed.Param.Optional {name; default} -> 
                let const, params = optional name.scope default in
                (destruct @ [const], params)
            | Typed.Param.Extension _ -> (raise Common.TODO)
        ) in
        let bl = block ~ctx Typed.Block.(bl.stmts) in
        let bl' = Ast.Block.{stmts = destruct @ bl.stmts} in
        Ast_util.Lambda.curried (params) (Ast.Expr.Block (bl'))
        (* let inner_lambda = Ast.Lambda.{params = convert_pattern Typed.Param.(first.value); block = block ~ctx Typed.Block.(bl.stmts)} in
        Ast.Expr.Lambda (List.fold ~init: inner_lambda ~f: (fun inner param -> 
            Ast.Lambda.{params = convert_pattern param.value; block = {stmts = [Ast.Block.Return Ast.Return.{expr=Ast.Expr.Lambda inner}]}}
        ) rest) *)
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
    in 
    Ast.Const.{ name = ident_value n.scope_name; expr = const_expr }

and modu_entries ~ctx entries = 
    List.filter_map entries ~f: (function
        | Typed.Node.Module.Import _ -> None
        | Typed.Node.Module.Binding node -> 
            Some (Ast.Block.Const (let_ ~ctx node))
        | Typed.Node.Module.Module m -> Some (Ast.Block.Const (modu ~ctx m))
    )

and exposed_entries exposed = 
    let entries = Map.to_alist exposed 
    |> List.fold ~init: [] ~f:(fun acc (key, data) ->
        let binding = (match Typed.Symbol.Module.(data.binding) with
        | None -> []
        | Some b -> 
            [Ast.Object.Kv (key, Ast.Expr.Ident (Ast.Ident.{value = b.id.name}))])
        in
        let modu = (match Typed.Symbol.Module.(data.modu) with
        | None -> []
        | Some b -> 
            [Ast.Object.Kv (key, Ast.Expr.Ident (Ast.Ident.{value = b.id.name}))])
        in acc @ binding @ modu
    ) in
    Ast.Block.Return (Ast.Return.{expr = Ast.Expr.Object (Ast.Object.{entries})})

and modu ~ctx n =
    let exposed = exposed_entries n.exposed in
    let modu = modu_entries ~ctx n.entries in
    Ast.Const.block Typed.Module.(n.scope_name) (modu @ [exposed])

let module_exports ~ctx root = 
    (Typed.Node.Module.(root.exposed)
        |> Map.to_alist ~key_order: (`Increasing))
    |> Util.Lists.flat_map ~f:(fun (name, entries) -> 
        let binding = (match Typed.Symbol.Module.(entries.binding) with
            | None -> [] 
            | Some b -> 
                [
                    ([Typed.Symbol.Resolved.make name (Some b.id)] 
                    |> resolve_ident ~ctx
                    |> Ast.Ident.make
                    |> Ast.Expr.ident
                    |> Ast.Assign.make (Ast.Ident.make @@ "exports." ^ (ident_value name))
                    |> Ast.Block.assign
                    )
                ]

        ) in
        let modu = (match Typed.Symbol.Module.(entries.modu) with
            | None -> [] 
            | Some b -> 
                [
                    ([Typed.Symbol.Resolved.make name (Some b.id)] 
                    |> resolve_ident ~ctx
                    |> Ast.Ident.make
                    |> Ast.Expr.ident
                    |> Ast.Assign.make (Ast.Ident.make @@ "exports." ^ (ident_value name))
                    |> Ast.Block.assign
                    )
                ]

        ) in modu @ binding
    )

let root_module ~ctx root = 
    modu_entries ~ctx (Typed.Node.Module.(root.entries)) 
        @ (module_exports ~ctx root)

(*
    apply order:
    const a = 1 //idx = expr
    const b = 2 //idx = expr
    const f = f' (b) (a)
*)