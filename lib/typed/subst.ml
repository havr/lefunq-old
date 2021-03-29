open Base
open Erro
open Node

let empty_subst = Map.empty (module String)

let new_subst var typ = 
    Map.add_exn empty_subst ~key: var ~data: typ 

let add_subst substs var typ = 
    Map.add_exn substs ~key: var ~data: typ 

let substs_to_string substs = Map.to_alist substs |> List.map ~f:(fun (k, v) ->
    (k ^ "=" ^ Type.to_string v)
) |> String.concat ~sep:", "

let concat_substs base other =
    Map.fold other ~init: base ~f:(fun ~key ~data result -> Map.set result ~key ~data)

let rec apply_substs substs = function
| Type.Var v -> Map.find substs v |> Option.value ~default: (Type.Var v) 
| Type.Simple (s, args) -> Type.Simple (s, List.map ~f:(apply_substs substs) args)
| Type.Lambda (from, to') -> Type.Lambda (apply_substs substs from, apply_substs substs to')
| Type.Tuple t -> Type.Tuple (List.map t ~f:(apply_substs substs))
| Type.Unknown -> Type.Unknown
| Type.Unit -> Type.Unit

let apply_substs_to_substs target substs = 
    Map.map target ~f: (fun v -> apply_substs substs v)

let combine_substs base other =
    concat_substs base (apply_substs_to_substs other base) 

let rec apply_to_pattern substs = function
    | Match.Any -> Match.Any
    | Match.Unit -> Match.Unit
    | Match.Str s -> Match.Str s
    | Match.Int i  -> Match.Int i
    | Match.Param p -> Match.Param {
        p with typ = apply_substs substs p.typ
    }
    | Match.Tuple t -> Match.Tuple (List.map t ~f: (apply_to_pattern substs));
    | Match.List li -> Match.List {
        items = List.map li.items ~f: (apply_to_pattern substs);
        rest = li.rest; (* TODO: apply substs to rest! *)
        item_typ = apply_substs substs li.item_typ;
    }

let apply_substs_to_params substs params = 
    let rec apply_param param = 
        let shape = match Param.(param.shape) with
            | Param.Tuple ts -> 
                Param.Tuple (List.map ts ~f:apply_param)
            | t -> t
        in let type' = apply_substs substs param.type' in
        Param.{type'; shape}
    in List.map params ~f:apply_param

let apply_substs_to_scheme substs scheme = Type.{scheme with typ = apply_substs substs scheme.typ}

let rec apply_substs_to_expr substs = function
(* apply_substs_to_type *)
    | Expr.Li li -> Expr.Li {li with items = List.map li.items ~f: (apply_substs_to_expr substs)}
    | Expr.Foreign f -> Expr.Foreign f
    | Expr.Value m -> 
        Expr.Value {m with type_ = apply_substs substs m.type_}
    | Expr.Tuple t -> 
        Expr.Tuple { t with exprs = List.map t.exprs ~f:(apply_substs_to_expr substs) }
    | Expr.Ident m -> 
        Expr.Ident ( match m.scheme with
            | None -> m
            | Some scheme -> { m with scheme = Some { scheme with typ = apply_substs substs scheme.typ}}
        )
    | Expr.Apply m -> 
        let fn = apply_substs_to_expr substs m.fn in
        let args = List.map m.args ~f:(apply_substs_to_expr substs) in
        Expr.Apply { m with fn; args}
    | Expr.Lambda lam -> 
        let params = apply_substs_to_params substs Lambda.(lam.params) in
        let block = apply_substs_to_block substs lam.block in
        Expr.Lambda Lambda.{lam with params; block}
    | Expr.Cond c -> 
        let cases = List.map Cond.(c.cases) ~f:(fun cas ->
            let if_ = apply_substs_to_block substs cas.if_ in
            let then_ = apply_substs_to_block substs cas.then_ in
            Cond.{if_; then_}
        ) in
        let else_ = Option.map c.else_ ~f:(apply_substs_to_block substs) in
        Expr.Cond Cond.{c with cases; else_}
    | Expr.Match m -> Expr.Match { m with 
            expr = apply_substs_to_expr substs m.expr;
            cases = List.map m.cases ~f: (fun case -> Match.{ 
                stmts = apply_substs_to_block_stmts substs case.stmts;
                pattern = apply_to_pattern substs case.pattern
            })
        }

and apply_substs_to_block_stmts substs stmts =  
        List.map stmts ~f: (function 
        | Stmt.Block b -> Stmt.Block {b with stmts = apply_substs_to_block_stmts substs b.stmts}
        | Stmt.Expr e -> Stmt.Expr (apply_substs_to_expr substs e)
        | Stmt.Let b -> 
            let scheme = Option.map b.scheme ~f:(apply_substs_to_scheme substs) in
            let block = apply_substs_to_block substs b.block in
            Stmt.Let {b with scheme; block }
    )
and apply_substs_to_block substs b =
        {b with stmts = apply_substs_to_block_stmts substs b.stmts}

let apply_to_error substs err = 
match err with
| Erro.IgnoredResult {unexpected; range} -> 
    Erro.IgnoredResult {unexpected = apply_substs substs unexpected; range}
| Erro.TypeMismatch { type_expected; type_provided; range} -> 
(* Common.log [
    "RNG"; Span.range_str range;
    "TE"; Type.to_string type_expected;
    "TP"; Type.to_string type_provided;
    "TE2"; Type.to_string @@ apply_substs substs type_expected;
    "TP2"; Type.to_string @@ apply_substs substs type_provided;
]; *)
    Erro.TypeMismatch {
        range;
        type_expected = apply_substs substs type_expected;
        type_provided = apply_substs substs type_provided;
    }
| Erro.NotFunction { type_provided; range } -> Erro.NotFunction {
    range;
    type_provided = apply_substs substs type_provided;
}
| Erro.IfTypeMismatch { unexpected; range } -> Erro.IfTypeMismatch {
    range;
    unexpected = apply_substs substs unexpected;
}
| BranchTypeMismatch { unexpected; expected; range } -> Erro.BranchTypeMismatch {
    range;
    unexpected = apply_substs substs unexpected;
    expected = apply_substs substs expected;
}
| PatternMismatch t -> PatternMismatch {
    t with 
        expected = apply_substs substs t.expected;
        unexpected = apply_substs substs t.unexpected
} 
| ListItemTypeMismatch t -> ListItemTypeMismatch {
    t with 
        expected = apply_substs substs t.expected;
        unexpected = apply_substs substs t.unexpected
} 
| UndeclaredIdentifier t -> UndeclaredIdentifier t
| CyclicDependency t -> CyclicDependency t
| SourceNotFound t -> SourceNotFound t
| SourceSymbolNotFound t -> SourceSymbolNotFound t
| SourceSystemError t -> SourceSystemError t
| SourceCompileError t -> SourceCompileError t
| NonExhaustivePatternMatching t -> NonExhaustivePatternMatching t
| UnusedMatchCase t -> UnusedMatchCase t
