open Base
open Node

let type_name ~ctx range name = 
    match Scope.lookup Infer_ctx.(ctx.scope) Type.(name.resolved) with
    | Some ({typedef = Some typedef; _}, resolved) -> 
        let qual = Typed_common.Qualified.resolve_name (typedef.id) resolved in
        Type.{
            resolved = qual;
            def = typedef.def;
        }
        
    | Some ({typedef = None; _}, _) -> 
    (* TODO: rly *)
        Infer_ctx.add_error ~ctx (Errors.UndeclaredIdentifier {given_name = Typed_common.Qualified.given name.resolved; range});
        name
    | None -> 
        Infer_ctx.add_error ~ctx (Errors.UndeclaredIdentifier {given_name = Typed_common.Qualified.given name.resolved; range});
        name

let rec type_ident ~ctx = function
    | TypeIdent.Simple {given; name; args} ->
        TypeIdent.Simple {given; args = List.map args ~f: (type_ident ~ctx); name = type_name ~ctx (given.range) name}
    | TypeIdent.Lambda {arg; result} ->
        TypeIdent.Lambda {
            arg = TypeIdent.{arg with typ = type_ident ~ctx arg.typ};
            result = type_ident ~ctx result
        }
    | TypeIdent.Var v -> TypeIdent.Var v
    | TypeIdent.Tuple {items} -> TypeIdent.Tuple {items = List.map items ~f:(type_ident ~ctx)}
    | TypeIdent.List li -> TypeIdent.List (type_ident ~ctx li)
    | TypeIdent.Unit -> TypeIdent.Unit

let rec type_from_ident = function
    | TypeIdent.Var {given} -> Type.Var given.value
    | TypeIdent.Simple {name; args; _} ->
        Type.Simple (name, List.map args ~f: type_from_ident)
    | TypeIdent.Lambda {arg; result} ->
        let from = match arg.label with
            | "" -> Type.PosParam (type_from_ident arg.typ)
            | label -> 
                Type.NamedParam {is_optional = arg.optional; param_name = label; param_typ = type_from_ident arg.typ}
        in
        Type.Lambda (from, type_from_ident result)
    | TypeIdent.Tuple {items} -> Type.Tuple (List.map items ~f:(type_from_ident))
    | TypeIdent.List li -> Base_types.list (type_from_ident li)
    | TypeIdent.Unit -> Type.Unit