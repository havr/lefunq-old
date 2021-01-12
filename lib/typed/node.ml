open Base

module rec Cond: sig 
    type case = {
        if_: Block.t;
        then_: Block.t
    }
    and t = {
        cases: case list;
        else_: Block.t option;
    }
end = struct 
    type case = {
        if_: Block.t;
        then_: Block.t
    }
    and t = {
        cases: case list;
        else_: Block.t option;
    }
end
and Apply: sig
    type t = { fn: Expr.t; args: Expr.t list }
end = struct 
    type t = { fn: Expr.t; args: Expr.t list }
end
and Value: sig
    type t = {value: string; type_: Type.t}
    val to_string: t -> string
    val equals: t -> t -> bool
end = struct 
    type t = {value: string; type_: Type.t}
    let to_string v = "value " ^ v.value ^ " :" ^ (Type.to_string v.type_)
    let equals a b = 
        let values_equal = phys_equal a.value b.value in
        let types_equal = Type.equals a.type_ b.type_ in
        values_equal && types_equal

end
and Ident: sig 
    type resolved = Local of {
        scope_name: string;
        param: bool
    } | Global of {
        global_name: string;
    } | Undefined

    type t = {
        pos: Common.Pos.t;
        given_name: string; 
        resolved: resolved;
        scheme: Type.scheme option;
    }

    val to_string: t -> string
    val equals: t -> t -> bool
end = struct 
    type resolved = Local of {
        scope_name: string;
        param: bool
    } | Global of {
        global_name: string;
    } | Undefined

    type t = {
        pos: Common.Pos.t;
        given_name: string; 
        resolved: resolved;
        scheme: Type.scheme option;
    }

    let equals a b =
        let given_name_equals = phys_equal a.given_name b.given_name in
        let resolved_equals = match (a.resolved, b.resolved) with
        | Global ag, Global bg -> phys_equal ag.global_name bg.global_name
        | Local al, Local bl ->
            (phys_equal al.scope_name bl.scope_name)
            && (phys_equal al.param bl.param)
        | Undefined, Undefined -> true
        | _, _ -> false
        in
        (Caml.(=) a.scheme b.scheme) && resolved_equals && given_name_equals

    let to_string id = 
        let (resolved, kind) = match id.resolved with
        | Local {scope_name; param} ->
            (scope_name, if param then "param" else "local")
        | Global {global_name} ->
            (global_name, "global")
        | Undefined -> 
            ("???", "undefined")
        in

        let scheme = match id.scheme with
        | None -> "unknown"
        | Some s -> Type.scheme_to_string s
        in

        String.concat [
            "IDENT"; resolved; "("; kind; " "; id.given_name; "): "; scheme
        ]

end
and Let: sig 
    type t = {
        given_name: string;
        scope_name: string;
        scheme: Type.scheme option;
        is_rec: bool;
        block: Block.t 
    }
end = struct 
    type t = {
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
end = struct 
    type t = Expr of Expr.t | Let of Let.t | Block of Block.t
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
    type t = {stmts: Stmt.t list}
    val equals: Block.t -> Block.t -> bool
    val to_string: Block.t -> string
end = struct 
    type t = {stmts: Stmt.t list}
    let to_string block = List.map block.stmts ~f:Stmt.to_string 
        |> String.concat ~sep: "\n"
    let equals a b = if List.length a.stmts <> List.length b.stmts then false else
        List.zip_exn a.stmts b.stmts 
        |> List.map ~f:(fun (sa, sb) -> Stmt.equals sa sb) 
        |> List.find ~f:(fun entry -> not entry) 
        |> Option.is_none
end
and Tuple: sig 
    type t = {exprs: Expr.t list}
end = struct
    type t = {exprs: Expr.t list}
end
and Lambda: sig 
    type t = {
        block: Block.t;
        params: Param.t list;
    }
end = struct 
    type t = {
        block: Block.t;
        params: Param.t list;
    }
end
and Expr: sig 
    type t = Value of Value.t | Ident of Ident.t | Apply of Apply.t | Lambda of Lambda.t | Cond of Cond.t | Tuple of Tuple.t
    val to_string: t -> string
end = struct 
    type t = Value of Value.t | Ident of Ident.t | Apply of Apply.t | Lambda of Lambda.t | Cond of Cond.t | Tuple of Tuple.t
    let to_string = function
    | Value v -> Value.to_string v
    | Ident id -> Ident.to_string id
    | Apply _ -> raise Common.TODO 
    | Lambda _ -> raise Common.TODO
    | Cond _ -> raise Common.TODO
    | Tuple _ -> raise Common.TODO
end

module Module = struct 
    type entry = Binding of Let.t
    type t = {
        name: string;
        entries: entry list
    }
end
