open Base
open Node
open Common
open Typed_common

module TyNode = Node
module AstNode = Ast.Node

let rec type_ident = function
    | AstNode.Type.Lambda {arg; result} -> 
        TyNode.TypeIdent.Lambda {
            arg = TyNode.TypeIdent.{optional = false; label = ""; typ = type_ident arg};
            result = type_ident result
        }
    (* TODO: type params *)
    | AstNode.Type.Var {name} -> TyNode.TypeIdent.Var {given = name}
    | AstNode.Type.Simple {name; args} -> 
        TyNode.TypeIdent.Simple {
            given = name;
            name = Type.make_name (Qualified.from_string name.value) Type.Unresolved;
            args = List.map args ~f:type_ident;
        }
    | AstNode.Type.Tuple {items} -> TyNode.TypeIdent.Tuple {items = items |> List.map ~f: type_ident}
    | AstNode.Type.List li -> TyNode.TypeIdent.List (type_ident li)
    | AstNode.Type.Unit -> TyNode.TypeIdent.Unit

let using u = 
    let rec nested path = Util.Lists.flat_map ~f: (function 
        | AstNode.Using.Wildcard _ -> [Using.Wildcard, path]
        | AstNode.Using.Ident {name; action} -> match (action) with
            | None -> [(Using.Only name),  path @ [name]]
            | Some (Rename newname) -> [(Using.Only newname), path @ [name]]
            | Some (Nested names) -> nested (path @ [name]) names
    ) in
    let root = match AstNode.Using.(u.kind) with
        | Local name -> Using.Local name 
        (* TODO: AstNode.Using.Source  *)
        | Global name -> Using.Source {name; resolved= ""}
    in
    let names = match u.action with
        | Nested items -> nested [] items
        | Rename rename -> [(Using.Only rename), []]
    in
    Using.{
        range = Span.empty_range;
        root;
        names;
        resolved = []
    }


(* let make_name given scope = Param.{given; scope} *)

let ident id = 
    let range = Span.(id.range) in
    Ident.{
        typ = Type.Unknown;
        range = range;
        scheme = None;
        qual = Qualified.from_string Span.(id.value);
    }

let rec destruct sh = match sh with
    | AstNode.Destruct.Name name -> TyNode.Destruct.Name (TyNode.Destruct.{given = name.value; scope = ""; typ = Type.Unknown})
    | AstNode.Destruct.Tuple tuple -> TyNode.Destruct.Tuple (List.map tuple ~f: destruct)
    | AstNode.Destruct.Unit -> TyNode.Destruct.Unit

let param ~expr p = 
    match p with
    | AstNode.Param.Positional p -> TyNode.Param.{
        value = TyNode.Param.Positional {shape = destruct p.shape};
        typ = Type.Unknown;
    }
    | AstNode.Param.Named p -> 
        let shape = match p.shape with
            | None -> TyNode.Destruct.Name (TyNode.Destruct.{given = p.name.value; scope = ""; typ = Type.Unknown})
            | Some shape -> destruct shape
        in
        TyNode.Param.{
            value = TyNode.Param.Named {given = p.name; shape};
            typ = Type.Unknown;
        }
    | AstNode.Param.Optional p -> TyNode.Param.{
        value = Optional {
            scope = "";
            given = p.name;
            alias = p.alias;
            default = Option.map ~f:expr p.default
        };
        typ = Type.Unknown;
    }
    | AstNode.Param.Extension p -> TyNode.Param.{
        value = Extension {
            given = p.name
        };
        typ = Type.Unknown;
    }

let params ~expr = List.map ~f:(param ~expr)

let typedef td =
    TyNode.Typedef.{
        name = AstNode.Typedef.(td.name);
        scope_name = "";
        params = List.map td.params ~f: (fun param -> {var = param.var});
        def = match td.def with
            | Foreign _ -> Foreign
    }

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
        scheme = Type.make_scheme [] Type.Unknown;
        type_ident = type_ident f.typ;
        range = f.name.range;
        name = f.name.value;
    }
    | Ast.Node.Value.Unit u -> Expr.Value Value.{typ = Base_types.unit; range = u.range; value = ""}
    | Ast.Node.Value.Int int -> Expr.Value Value.{typ = Base_types.int; range = int.range; value = int.value}
    | Ast.Node.Value.Str str -> Expr.Value Value.{typ = Base_types.str; range = str.range; value = str.value}
    (* TODO: return AST without parlex pos *)
    | Ast.Node.Value.Ident id -> Expr.Ident (ident id)
    | Ast.Node.Value.Tuple tu ->
        Expr.Tuple (Tuple.{typ = Type.Unknown; range = tu.range; exprs = List.map ~f:expr tu.exprs })
    | Ast.Node.Value.Li li -> 
        let item = function
            | AstNode.Li.Single e -> TyNode.Li.Single (expr e)
            | AstNode.Li.Spread e -> TyNode.Li.Spread (expr e)
        in
        Expr.Li Li.{typ = Type.Unknown; range = li.range; items = List.map ~f:item li.items}
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
    | AstNode.Block.Using t -> Stmt.Using (using t)

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
        sigt = None; (* TODO: remove *)
        (* sigt = Option.map ~f:(typ) n.sig'; *)
        params = params;
        result = Type.Unknown
    }

and modu_entries = List.map ~f: (function
    | AstNode.Module.Typedef td -> Module.Typedef (typedef td)
    | AstNode.Module.Module m -> Module.Module (modu m)
    | AstNode.Module.Let t -> Module.Binding (binding t)
    | AstNode.Module.Using u -> Module.Using (using u)
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
