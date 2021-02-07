open Base
open Common

module rec Cond: sig 
    type case = {
        if_: Block.t;
        then_: Block.t
    }
    and t = {
        range: Span.range;
        cases: case list;
        else_: Block.t option;
    }
end = struct 
    type case = {
        if_: Block.t;
        then_: Block.t
    }
    and t = {
        range: Span.range;
        cases: case list;
        else_: Block.t option;
    }
end
and Apply: sig
    type t = {
        range: Span.range;
        fn: Expr.t;
        args: Expr.t list;
    }
end = struct 
    type t = {
        range: Span.range;
        fn: Expr.t;
        args: Expr.t list; 
    }
end
and Value: sig
    type t = {
        value: string;
        type_: Type.t;
        range: Span.range
    }

    val to_string: t -> string
    val equals: t -> t -> bool
end = struct 
    type t = {
        value: string;
        type_: Type.t;
        range: Span.range
    }

    let to_string v = "value " ^ v.value ^ " :" ^ (Type.to_string v.type_)
    let equals a b = 
        let values_equal = phys_equal a.value b.value in
        let types_equal = Type.equals a.type_ b.type_ in
        values_equal && types_equal

end
and Ident: sig 
    type t = {
        range: Span.range;
        given_name: string; 
        resolved: Symbol.Id.t option;
        scheme: Type.scheme option;
    }

    val to_string: t -> string
    val equals: t -> t -> bool
end = struct 
    type t = {
        range: Span.range;
        given_name: string; 
        resolved: Symbol.Id.t option;
        scheme: Type.scheme option;
    }

    let equals a b =
        let given_name_equals = phys_equal a.given_name b.given_name in
        let resolved_equals = match (a.resolved, b.resolved) with
        | None, None -> true
        | Some a, Some b -> Symbol.Id.equals a b
        | _, _ -> false
        in
        (Caml.(=) a.scheme b.scheme) && resolved_equals && given_name_equals

    let to_string id = 
        let resolved = match id.resolved with
        | Some id ->
            (Symbol.Id.to_string id)
        | None -> 
            ("???")
        in

        let scheme = match id.scheme with
        | None -> "unknown"
        | Some s -> Type.scheme_to_string s
        in

        String.concat [
            "IDENT"; resolved; "("; id.given_name; "): "; scheme
        ]

end
and Let: sig 
    type t = {
        range: Span.range;
        given_name: string;
        scope_name: string;
        scheme: Type.scheme option;
        is_rec: bool;
        block: Block.t 
    }
end = struct 
    type t = {
        range: Span.range;
        given_name: string;
        scope_name: string;
        scheme: Type.scheme option;
        is_rec: bool;
        block: Block.t 
    }
end

and Stmt: sig 
    (* todo: let stmt and block stmt*)
    type t = Expr of Expr.t | Let of Let.t | Block of Block.t
    val equals: Stmt.t -> Stmt.t -> bool
    val to_string: Stmt.t -> string
    val range: Stmt.t -> Span.range
end = struct 
    type t = Expr of Expr.t | Let of Let.t | Block of Block.t
    let range = function 
    | Expr e -> Expr.range e
    | Let let' -> let'.range
    | Block b -> b.range

    let equals a b = match (a, b) with
    | Expr _, Expr _ -> true (* TODO *)
    | Let _, Let _ -> true
    | Block ba, Block bb -> Block.equals ba bb
    | _, _ -> false

    let to_string = function
    | Expr e -> Expr.to_string e
    | Let b -> String.concat ["LET "; b.scope_name; "("; b.given_name; ")"; "\n"; Block.to_string b.block]
    | Block _ -> "block"
end
and Block: sig 
    type t = {range: Span.range; stmts: Stmt.t list}
    val equals: Block.t -> Block.t -> bool
    val to_string: Block.t -> string
    val last_stmt_range: Block.t -> Span.range
end = struct 
    type t = {range: Span.range; stmts: Stmt.t list}

    let last_stmt_range bl = match bl.stmts with
        | [] -> bl.range
        | stmts -> Stmt.range (List.last_exn stmts)

    let to_string block = List.map block.stmts ~f:Stmt.to_string 
        |> String.concat ~sep: "\n"
    let equals a b = if List.length a.stmts <> List.length b.stmts then false else
        List.zip_exn a.stmts b.stmts 
        |> List.map ~f:(fun (sa, sb) -> Stmt.equals sa sb) 
        |> List.find ~f:(fun entry -> not entry) 
        |> Option.is_none
end
and Tuple: sig 
    type t = {
        range: Span.range;
        exprs: Expr.t list
    }
end = struct
    type t = {
        range: Span.range;
        exprs: Expr.t list
    }
end
and Lambda: sig 
    type t = {
        range: Span.range;
        block: Block.t;
        params: Param.t list;
    }
end = struct 
    type t = {
        range: Span.range;
        block: Block.t;
        params: Param.t list;
    }
end
and Expr: sig 
    type t = 
        | Value of Value.t 
        | Ident of Ident.t
        | Apply of Apply.t
        | Lambda of Lambda.t
        | Cond of Cond.t
        | Tuple of Tuple.t

    val to_string: t -> string
    val range: t -> Span.range
end = struct 
    type t = 
        | Value of Value.t
        | Ident of Ident.t
        | Apply of Apply.t
        | Lambda of Lambda.t
        | Cond of Cond.t
        | Tuple of Tuple.t

    let range = function
    | Value v -> v.range
    | Ident id -> id.range
    | Apply app -> app.range
    | Lambda lam -> lam.range
    | Cond cond -> cond.range
    | Tuple tup -> tup.range

    let to_string = function
    | Value v -> Value.to_string v
    | Ident id -> Ident.to_string id
    | Apply _ -> raise Common.TODO 
    | Lambda _ -> raise Common.TODO
    | Cond _ -> raise Common.TODO
    | Tuple _ -> raise Common.TODO
end

module Import = struct 
    type name = {
        name: string Span.t;
        path: string Span.t list;
        resolved: Resolver.Scope.resolution option;
    }

    type t = {
        resolved_source: string;
        source: string Span.t;
        names: name list;
    }

    let pretty_print n = 
        let open Pp in
        let exact = List.map n.names ~f: (fun m -> 
            let resolved = match m.resolved with
                | None -> []
                | Some {typedef; modu; binding} -> 
                    Option.map typedef ~f:(fun _ -> branch [text "typedef"] [])
                    :: Option.map modu ~f:(fun m -> branch [text "module"; text (Symbol.Id.to_string m.id)] [])
                    :: Option.map binding ~f:(fun m -> branch [
                        text "binding";
                        text (Symbol.Id.to_string m.exposed);
                        text "=>";
                        text (Symbol.Id.to_string m.internal)
                    ] [])
                    :: []
                    |> List.filter ~f: (Option.is_some)
                    |> List.map ~f: (fun op -> Option.value_exn op)
            in
            branch [spanned m.name; m.path |> List.map ~f: (fun s -> Span.(s.value)) |> String.concat |> text] resolved
        ) in
        branch [text "import"; spanned n.source] exact
end

module Module = struct 
    type entry = Binding of Let.t | Import of Import.t
    type t = {
        name: string;
        entries: entry list
    }
end
