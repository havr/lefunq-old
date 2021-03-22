open Base
open Node

module TyNode = Node
module AstNode = Ast.Node

let rec typ = function
    | AstNode.Type.Lambda {arg; result} -> Type.Lambda (typ arg, typ result)
    (* TODO: type params *)
    | AstNode.Type.Simple {name; _} -> Type.Simple (name.value, [])
    | AstNode.Type.Tuple {items} -> Type.Tuple (items |> List.map ~f: typ)
    | AstNode.Type.Unit -> Base_types.unit 

let import import = 
    let rec nested_names path names = 
        List.concat @@ List.map names ~f:(fun entry ->
            match (Ast.Node.Import.(entry.rename), entry.nested) with
            | None, None -> [ TyNode.Import.{
                    name = entry.name;
                    path = path @ [entry.name];
                    resolved = None
                } ]
            | Some rename, None -> [ TyNode.Import.{
                    name = rename;
                    path = path @ [rename];
                    resolved = None
                } ]
            | None, Some nested ->
                nested_names (path @ [entry.name]) nested
            | Some _, Some _-> raise Common.Unexpected
        ) 
    in TyNode.Import.{
        source = Ast.Node.Import.(import.source.name);
        resolved_source = "";
        names = match import.source.nested with
        | Some nested -> nested_names [] nested
        | None -> []
    }
    (* let names = match AstNode.Import.(import.name) with
    | None -> []
    | Some n -> [TyNode.Import.{name = n; path=[]; resolved=None}]
    in TyNode.Import.{
        source = import.source;
        names = names 
    } *)


let params args = 
    let rec map_arg = function
    | Ast.Node.Arg.Ident m -> 
        Param.{shape = Param.Name (Param.{given = m.ident; resolved = ""}); type' = Type.Unknown}
    | Ast.Node.Arg.Tuple m -> begin 
        match m.tuple with 
        | [] -> Param.{shape = Param.Unit; type' = Base_types.unit}
        | name :: [] -> map_arg name
        | tuple ->
            Param.{shape = Param.Tuple (List.map tuple ~f:map_arg); type' = Type.Unknown}
        end 
    in List.map args ~f:map_arg

let rec apply apply = 
    let fn = expr Ast.Node.Apply.(apply.fn) in
    let args = List.map ~f:expr Ast.Node.Apply.(apply.args) in
        Node.Apply.{fn=fn; args=args; range = apply.range}
and value = function
    | Ast.Node.Value.Foreign f -> Expr.Foreign (Foreign.{range = f.range; name = f.value; scheme = None})
    | Ast.Node.Value.Unit u -> Expr.Value (Value.{range = u.range; value = ""; type_ = Base_types.unit})
    | Ast.Node.Value.Int int -> Expr.Value (Value.{range = int.range; value = int.value; type_ = Base_types.int})
    | Ast.Node.Value.Str str -> Expr.Value (Value.{range = str.range; value = str.value; type_ = Base_types.str})
    (* TODO: return AST without parlex pos *)
    | Ast.Node.Value.Ident id -> 
        let (resolved, resolution) = (match id.value
        |> String.split ~on: '.' 
        |> List.map ~f: (fun value -> Symbol.Resolved.make value None)
        |> List.rev with
        | resolved :: resolution -> (resolved, List.rev resolution)
        | [] -> raise Common.Unreachable
        ) in Expr.Ident (Ident.{
            range = id.range;
            scheme = None;
            resolved;
            resolution;
        })
    | Ast.Node.Value.Tuple tu ->
        Expr.Tuple (Tuple.{
            range = tu.range;
            exprs = List.map ~f:expr tu.exprs
        })
    | Ast.Node.Value.Li li -> 
        Expr.Li (Li .{
            range = li.range;
            items = List.map ~f:expr li.items;
        })
    | Ast.Node.Value.Lambda lam -> 
        let b = block lam.block in
        let a = params lam.args.args in
            Expr.Lambda (Lambda.{range = lam.range; params = a; block = b})
    
and cond n = 
    let expr_or_block = function
        (* TODO: variable names *)
        | Ast.Node.Cond.Expr e -> Block.{
            stmts = [Stmt.Expr (expr e)];
            range = Ast.Node.Expr.range e
        }
        | Ast.Node.Cond.Block b -> block b
    in
    let case = Cond.{if_ = expr_or_block Ast.Node.Cond.(n.if_); then_ = expr_or_block Ast.Node.Cond.(n.then_)} in
    match n.else_ with
    | None ->
        Cond.{cases = [case]; else_ = None; range = n.range}
    | Some (Ast.Node.Cond.Expr (Ast.Node.Expr.Cond e)) ->
            let result = cond e in 
            {result with cases = case :: result.cases}
    | Some m -> Cond.{cases = [case]; else_ = Some (expr_or_block m); range = n.range}
and expr: Ast.Node.Expr.t -> Node.Expr.t = function
    | Ast.Node.Expr.Value v -> value v
    | Ast.Node.Expr.Apply app -> Expr.Apply (apply app)
    | Ast.Node.Expr.Cond n -> Expr.Cond (cond n)
and block_stmt = function
    | Ast.Node.Block.Expr e -> Stmt.Expr (expr e)
    | Ast.Node.Block.Block b -> Stmt.Block (block b)
    | Ast.Node.Block.Let t -> Stmt.Let (binding t)
and block b = Block.{stmts = (List.map ~f:block_stmt b.stmts); range = b.range}
and binding_expr = function
    | Ast.Node.Let.Expr v -> Block.{stmts = [Stmt.Expr (expr v)]; range = AstNode.Expr.range v}
    | Ast.Node.Let.Block e -> block e
and binding n = 
    (* todo: s/args/params in paraser*)
    let params = match n.args with
        | None -> []
        | Some {args} -> params args
    in
    Let.{
        range = n.range;
        given_name = n.ident.value;
        scope_name = "";
        block = binding_expr n.expr;
        is_rec = n.is_rec;
        scheme = None;
        (* use sigt everywhere *)
        sigt = Option.map ~f:(typ) n.sig';
        params = params;
        result = Type.Unknown
    }
and modu_entries entries = List.map entries ~f: (function
    | Ast.Node.Module.Module m -> Module.Module (modu m)
    | Ast.Node.Module.Let t -> Module.Binding (binding t)
    | Ast.Node.Module.Import i -> Module.Import (import i)
)
and modu m = 
    let entries = modu_entries Ast.Node.Module.(m.entries) in Module.{
        given_name = m.name.value;
        scope_name = "";
        entries;
        exposed = Map.empty(module String)
    }
and root m = 
    let entries = modu_entries Ast.Node.Root.(m.entries) in Module.{
        given_name = "";
        scope_name = ""; 
        entries;
        exposed = Map.empty(module String)
    }