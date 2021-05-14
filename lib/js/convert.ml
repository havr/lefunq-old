open Base
open Convert_common

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
    '<', "$less";
    '+', "$add";
    '-', "$sub";
    '*', "$mul";
    '/', "$div";
    '$', "$dollar";
    '|', "$pipe";
    '>', "$more";
    '=', "$eq"
]

module Tn = Typed.Node

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
        List.fold ["'", "$prime"; "!", "$unsafe"; "=", "$o_O"] ~init: name ~f: (fun accum (str, sub) -> 
            String.substr_replace_all accum ~pattern: str ~with_: sub)

module Dsl = struct 
    let apply fn args = Ast.Expr.Apply Ast.Apply.{fn; args}

end

open Common

type ctx = {
    source: string;
    required_sources: string StringMap.t
}

let resolve_ident ~ctx name = 
    match Typed.Id.(name.source) with
    | "" -> ident_value (name.name)
    | src ->
        let reqname = if (String.equal src ctx.source) then "" else (Map.find_exn ctx.required_sources src) ^ "." in
        let mods = match name.modules with
            | [] -> ""
            | modules -> (String.concat ~sep:"." modules) ^ "." in
        let name = name.name in
        reqname ^ mods ^ (ident_value name)

let ident ~ctx n = 
    let id = Option.value_exn ~message: 
        Tn.Ident.("No resolution for:" ^ (Typed.Typed_common.Qualified.given n.qual)) n.qual.name.resolved in
    Ast.Ident.{ value = resolve_ident ~ctx id }

let basic b =
    if Typed.Type.equals Tn.Value.(b.typ) Typed.Base_types.str 
        then Ast.Expr.Str(Ast.Str.{value = b.value})
    else if Typed.Type.equals Tn.Value.(b.typ) Typed.Base_types.int 
        then Ast.Expr.Num(Ast.Num.{value = b.value})
    else if Typed.Type.equals Tn.Value.(b.typ) Typed.Base_types.unit 
        then Ast.Expr.Void
    else raise @@ Invalid_argument ("unexpected basic type: " ^ (Typed.Type.to_string b.typ) ^ "(has value: " ^ b.value ^ ")")

let exports names = 
    Ast.Object.{
        entries = List.map names ~f: (fun name -> Ast.Object.Pun name)
    }

let rec apply ~ctx n = begin
    let open Typed.Node in
    match Apply.(n.fn) with 
    | Expr.Ident m -> 
        (* TODO: !!FQN like!! import.value *)
        let local_name = (Option.value_exn m.qual.name.resolved).name in
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

end and expr ~ctx n = 
    let open Typed.Node in
    match n with
    | Expr.Li li -> Ast.Expr.Li (List.map li.items ~f: (expr ~ctx))
    | Expr.Value m -> basic m
    | Expr.Ident m -> Ast.Expr.Ident (ident ~ctx m)
    | Expr.Apply m -> apply ~ctx m
    | Expr.Lambda m -> lambda ~ctx m
    | Expr.Match m -> 
        let stmt_to_expr = function
        | Ast.Block.Expr expr -> expr
        | stmt -> Ast.Expr.Block (Ast.Block.{stmts=[stmt]})
        in
        let cond = Convert_match.match' 
            ~stmts: (block_stmts ~ctx)
            ~map_return: (fun stmt -> Ast.Block.Return (Ast.Return.{expr = stmt_to_expr stmt}))
            (Ast_util.Expr.ident "__TODO")
            m.cases
        in
        Ast_util.Lambdas.scoped ["__TODO", expr ~ctx m.expr] [Ast.Block.Cond cond]
    | Cond m -> cond ~ctx m
    | Tuple t -> Ast.Expr.Li (List.map t.exprs ~f: (expr ~ctx))
    | Foreign f -> Ast.Expr.Ident Ast.Ident.{value = foreign_require ^ "." ^ f.name}
and cond ~ctx n = 
    let open Typed.Node in
    let cond_expr = function 
        | (Stmt.Expr e) :: [] -> expr ~ctx e 
        | b -> Block (block ~ctx b)
    in
    let conds = Cond.(n.cases) |> List.map ~f:(fun Cond.{if_; then_} ->
        let if_ = cond_expr if_.stmts in
        let then_ = block ~ctx then_.stmts in
        Ast.Cond.{if_ = if_; then_ = then_.stmts}
    ) in let cond_block = Ast.Block.Cond Ast.Cond.{
        conds=conds; else_ = n.else_ |> Option.map ~f: (fun m  -> Block.(block ~ctx m.stmts).stmts)
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
        let const = [Ast.Const.expr name cond] in
        (const, v)
    in
    match params with
    | [] -> raise (Invalid_argument "lambda without arguments")
    | params -> 
        let rec destruct_tuple acc tup = tup
            |> List.mapi ~f: (fun idx elem ->
                let curr_acc = Ast.Expr.Index (Ast.Index.make acc (Ast.Expr.Num (Ast.Num.from_int idx))) in
                destruct_shape curr_acc elem
            ) 
            |> Util.Lists.flatten
        and destruct_shape acc shape = 
            let open Typed.Node in
            match shape with 
            | Destruct.Name n -> 
                [Ast.Const.make n.scope (Ast.Const.Expr acc)]
            | Destruct.Tuple tup -> destruct_tuple acc tup 
            | Destruct.Unit -> []
        in
        let destruct_param_shape shape = 
            let open Typed.Node in
            match shape with
            | Destruct.Unit -> ([], "")
            | Destruct.Name n ->
                [], n.scope
            | Destruct.Tuple tup -> 
                let accname = tv () in
                let accn = Ast.Expr.Ident (Ast.Ident.make accname) in
                let bindings = destruct_tuple accn tup in
                (bindings, accname)
        in
        let destruct_param param = 
            let open Typed.Node in
            match param with
            | Param.Positional {shape} ->
                destruct_param_shape shape
            | Param.Named {shape; _} ->
                destruct_param_shape shape
            | Param.Optional {scope; default; _} ->
                optional scope default
            | Param.Extension _ -> (raise Common.TODO)
        in
        let destruct, params = List.fold_map params ~init: [] ~f: (fun destruct param -> 
            let stmts, param = destruct_param Typed.Node.Param.(param.value) in
            (destruct @ (List.map stmts ~f: (fun s -> Ast.Block.Const s))), param
        ) in
        let bl = block ~ctx Typed.Node.Block.(bl.stmts) in
        let bl' = Ast.Block.{stmts = destruct @ bl.stmts} in
        Ast_util.Lambda.curried (params) (Ast.Expr.Block (bl'))
        (* let inner_lambda = Ast.Lambda.{params = convert_pattern Typed.Param.(first.value); block = block ~ctx Typed.Block.(bl.stmts)} in
        Ast.Expr.Lambda (List.fold ~init: inner_lambda ~f: (fun inner param -> 
            Ast.Lambda.{params = convert_pattern param.value; block = {stmts = [Ast.Block.Return Ast.Return.{expr=Ast.Expr.Lambda inner}]}}
        ) rest) *)
and lambda ~ctx n = lambda_like ~ctx Typed.Node.Lambda.(n.params) n.block

and block_stmts ~ctx stmts = List.filter_map stmts ~f:(function
    | Typed.Node.Stmt.Expr n -> Some (Ast.Block.Expr (expr ~ctx n))
    | Typed.Node.Stmt.Block n -> Some (Ast.Block.Block (block ~ctx n.stmts))
    | Typed.Node.Stmt.Let n -> Some (Ast.Block.Const (let_ ~ctx n))
    | Typed.Node.Stmt.Using _ -> None
)

and block ~ctx n =
    let stmts = 
        block_stmts ~ctx n in
    let len = List.length stmts in
    let stmts' = stmts |> List.mapi ~f: (fun idx stmt ->
        if phys_equal idx (len - 1) then 
            match stmt with
            | Ast.Block.Expr expr -> Ast.Block.Return Ast.Return.{ expr }
            | t -> t
        else stmt
    )
    in Ast.Block.{ stmts = stmts' }

and const_expr ~ctx n = 
    match Tn.Let.(n.params) with
    | [] ->
        (match Tn.Block.(n.block.stmts) with
        | Tn.Stmt.Expr m :: [] -> Ast.Const.Expr (expr ~ctx m)
        | _ -> Ast.Const.Block  (block ~ctx n.block.stmts))
    | params -> Ast.Const.Expr (lambda_like ~ctx params n.block)

and assign_expr ~ctx n = 
    match Tn.Let.(n.params) with
    | [] ->
        (match Tn.Block.(n.block.stmts) with
        | Tn.Stmt.Expr m :: [] -> (expr ~ctx m)
        | _ -> Ast.Expr.Block (block ~ctx n.block.stmts))
    | params -> (lambda_like ~ctx params n.block)

and let_ ~ctx n = 
    (* for nested modules we truncate module path*)
    Ast.Const.{ name = ident_value n.scope_name; expr = const_expr ~ctx n}

and modu_entries ~ctx prefix entries = 
    let open Typed.Node in
    let make_assign name expr = 
        let name = ident_value name in
        match prefix with
        | "" ->
            Ast.Block.Const (Ast.Const.expr name expr), name
        | prefix ->
            let prefixed = prefix ^ "." ^ (name) in
            let assign = Ast.Block.Assign (Ast.Assign.make (Ast.Ident.make prefixed) expr) in
            assign, prefixed
    in
    let nodes, names = List.filter_map entries ~f: (function
        | Module.Using _ -> None
        | Module.Binding node -> 
            let assign, name = make_assign node.scope_name (assign_expr ~ctx node) in
            Some ([assign], name)
        | Module.Module node -> 
            let assign, name = make_assign node.scope_name (Ast.Expr.Object (Ast.Object.make [])) in
            let entries, _ = modu_entries ~ctx name node.entries in
            Some ((assign :: entries), name)
        | Module.Typedef _ -> None  (* TODO: process it *)
    ) |> List.unzip
    in (Util.Lists.flatten nodes, names)

let root_module source foreign_bindings root = 
    let deps, required_sources = Convert_using.require_nodes ~foreign_bindings ~source root in
    let ctx = {
        source; required_sources
    } in
    let nodes, names = modu_entries ~ctx "" (Typed.Node.Module.(root.entries)) in
    let exp = List.map names ~f: (fun name ->
        Ast.Block.Assign (Ast.Assign.make 
            (Ast.Ident.make("exports." ^ (name))) 
            (Ast.Expr.Ident (Ast.Ident.make (name))))
    ) in deps @ nodes @ exp

(*
    apply order:
    const a = 1 //idx = expr
    const b = 2 //idx = expr
    const f = f' (b) (a)
*)