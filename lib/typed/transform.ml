open Base
open Node
open Common

module TyNode = Node
module AstNode = Ast.Node

let rec typ = function
    | AstNode.Type.Lambda {arg; result} -> Type.Lambda (Type.PosParam (typ arg), typ result)
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


let make_name given scope = Param.{given; scope}

let ident id = 
    let resolved, resolution = 
        Span.(id.value)
        |> String.split ~on: '.' 
        |> List.map ~f: (fun value -> Symbol.Resolved.make value None)
        |> Util.Lists.last_rest in Ident.{
            typ = Type.Unknown;
            range = id.range;
            resolved;
            resolution;
        }

let param ~expr p = 
    let rec shape = function
    | AstNode.Param.Ident m -> 
        TyNode.Param.Name (make_name m.value "")
    | AstNode.Param.Tuple m -> (
        match m with 
        | [] -> TyNode.Param.Unit 
        | s :: [] -> shape s
        | tu -> Param.Tuple (List.map tu ~f:shape)
    ) in
    match p with
    | AstNode.Param.Positional p -> TyNode.Param.{
        value = Positional {pattern = shape p.shape};
        typ = Option.map ~f:typ p.typ 
            |> Option.value ~default: Type.Unknown 
    }
    | AstNode.Param.Named p -> TyNode.Param.{
        value = Named {name = name p.name.value p.name.value};
        typ = Option.map ~f:typ p.typ 
            |> Option.value ~default: Type.Unknown 
    }
    | AstNode.Param.Optional p -> TyNode.Param.{
        value = Optional {
            name = name p.name.value p.name.value;
            default = Option.map ~f:expr p.expr
        };
        typ = Option.map ~f:typ p.typ 
            |> Option.value ~default: Type.Unknown;
    }
    | AstNode.Param.Extension p -> TyNode.Param.{
        value = Extension {name = name p.name.value p.name.value};
        typ = typ p.typ
    }

let params ~expr = List.map ~f:(param ~expr)

let rec apply apply = 
    let fn = expr AstNode.Apply.(apply.fn) in
    let args = List.map ~f:arg apply.args in
        Node.Apply.{
            typ = Type.Unknown;
            range = apply.range;
            fn=fn;
            args=args
        }

and arg = function
    | AstNode.Apply.PosArg {expr = e} -> TyNode.Apply.PosArg {expr = expr e}
    | AstNode.Apply.NameArg {name; expr = e} -> TyNode.Apply.NameArg {name; expr = expr e}
    | AstNode.Apply.PunArg {name} -> TyNode.Apply.NameArg{name = name; expr = Expr.Ident (ident name)}

and value = function
    | Ast.Node.Value.Foreign f -> Expr.Foreign Foreign.{
        typ = Type.Unknown;
        range = f.range;
        name = f.value;
        scheme = None
    }
    | Ast.Node.Value.Unit u -> Expr.Value Value.{typ = Base_types.unit; range = u.range; value = ""}
    | Ast.Node.Value.Int int -> Expr.Value Value.{typ = Base_types.int; range = int.range; value = int.value}
    | Ast.Node.Value.Str str -> Expr.Value Value.{typ = Base_types.str; range = str.range; value = str.value}
    (* TODO: return AST without parlex pos *)
    | Ast.Node.Value.Ident id -> Expr.Ident (ident id)
    | Ast.Node.Value.Tuple tu ->
        Expr.Tuple (Tuple.{typ = Type.Unknown; range = tu.range; exprs = List.map ~f:expr tu.exprs })
    | Ast.Node.Value.Li li -> 
        Expr.Li Li.{typ = Type.Unknown; range = li.range; items = List.map ~f:expr li.items }
    | AstNode.Value.Lambda lam -> 
        let b = block lam.block in
        let a = params ~expr lam.params in
        Expr.Lambda Lambda.{typ = Type.Unknown; range = lam.range; params = a; block = b}
    
and cond n = 
    let expr_or_block = function
        (* TODO: variable names *)
        | AstNode.Cond.Expr e -> Block.{
            range = AstNode.Expr.range e;
            stmts = [Stmt.Expr (expr e)];
        }
        | AstNode.Cond.Block b -> block b
    in
    let case = Cond.{
        if_ = expr_or_block AstNode.Cond.(n.if_);
        then_ = expr_or_block AstNode.Cond.(n.then_)
    } in
    match n.else_ with
    | None ->
        Cond.{
            typ = Type.Unknown;
            range = n.range;
            cases = [case];
            else_ = None
        }
    | Some (AstNode.Cond.Expr (AstNode.Expr.Cond e)) ->
        let result = cond e in
        Cond.{
            result with 
            typ = Type.Unknown;
            range = n.range;
            cases = case :: result.cases
        }

    | Some m -> Cond.{
        typ = Type.Unknown;
        range = n.range;
        cases = [case];
        else_ = Some (expr_or_block m)
    }

and expr = function
    | AstNode.Expr.Value v -> value v
    | AstNode.Expr.Apply app -> Expr.Apply (apply app)
    | AstNode.Expr.Cond n -> Expr.Cond (cond n)
    | AstNode.Expr.Match m -> Expr.Match (matc m)

and matc n = 
    let rec pattern = function
        | AstNode.Match.Int i -> Match.Int i.value
        | AstNode.Match.Str s -> Match.Str s.value
        | AstNode.Match.Param p -> Match.Param {
            given_name = p.value;
            scope_name = p.value;
            typ = Type.Unknown;
        }
        | AstNode.Match.Tuple tup -> Match.Tuple (List.map tup ~f:pattern)
        | AstNode.Match.List li -> Match.List {
            items = List.map li.items ~f:pattern;
            rest = Option.map ~f: pattern li.rest;
            item_typ = Type.Unknown;
        } 
    in
    let cases = List.map AstNode.Match.(n.block.cases) ~f:(fun case ->
        Match.{
            pattern = pattern case.pattern;
            stmts = List.map case.stmts ~f:block_stmt
        }
    ) in Match.{
        typ = Type.Unknown;
        range = n.range;
        cases;
        expr = expr n.expr
    }

and block_stmt = function
    | AstNode.Block.Expr e -> Stmt.Expr (expr e)
    | AstNode.Block.Block b -> Stmt.Block (block b)
    | AstNode.Block.Let t -> Stmt.Let (binding t)

and block b = Block.{stmts = List.map ~f:block_stmt b.stmts; range = b.range}

and binding_expr = function
    | AstNode.Let.Expr v -> Block.{
        stmts = [Stmt.Expr (expr v)];
        range = AstNode.Expr.range v
    }
    | AstNode.Let.Block e -> block e

and binding n = 
    let params = Option.value ~default: [] (Option.map n.params ~f:(params ~expr)) in
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

and modu_entries = List.map ~f: (function
    | AstNode.Module.Module m -> Module.Module (modu m)
    | AstNode.Module.Let t -> Module.Binding (binding t)
    | AstNode.Module.Import i -> Module.Import (import i)
)

and modu m = 
    Module.{
        given_name = m.name.value;
        scope_name = "";
        entries = modu_entries AstNode.Module.(m.entries);
        exposed = Map.empty(module String)
    }

and root m = Module.{
        given_name = "";
        scope_name = ""; 
        entries = modu_entries AstNode.Root.(m.entries);
        exposed = Map.empty(module String)
    }
