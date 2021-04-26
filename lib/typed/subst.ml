open Base
open Node

let empty = Map.empty (module String)

let single var typ = 
    Map.add_exn empty ~key: var ~data: typ 

let add substs var typ = 
    Map.add_exn substs ~key: var ~data: typ 

let to_string substs = 
    Map.to_alist substs 
    |> List.map ~f:(fun (k, v) -> (k ^ "=" ^ Type.to_string v)) 
    |> String.concat ~sep:", "

let concat base other =
    Map.fold other ~init: base ~f:(fun ~key ~data result -> 
        Map.set result ~key ~data)

let rec apply substs = function
| Type.Invalid  -> Type.Invalid
| Type.Var v -> 
    Map.find substs v 
    |> Option.value ~default: (Type.Var v) 
| Type.Simple (s, args) -> 
    Type.Simple (s, List.map ~f:(apply substs) args)
| Type.Lambda (PosParam from, to') -> 
    Type.Lambda (PosParam (apply substs from), apply substs to')
| Type.Lambda (NamedBlock b, to') -> 
    Type.Lambda (NamedBlock (List.map b ~f: (fun p -> Type.{ p with param_typ = apply substs p.param_typ})), apply substs to')
| Type.Lambda (NamedParam p, to') -> 
    Type.Lambda (NamedParam ({p with param_typ = apply substs p.param_typ}), apply substs to')
| Type.Tuple t -> Type.Tuple (List.map t ~f:(apply substs))
| Type.Unknown -> Type.Unknown
| Type.Unit -> Type.Unit

let apply_to_substs target substs = 
    Map.map target ~f: (fun v -> apply substs v)

let combine base other =
    concat base (apply_to_substs other base) 

let rec apply_to_pattern substs = function
    | Match.Any -> Match.Any
    | Match.Unit -> Match.Unit
    | Match.Str s -> Match.Str s
    | Match.Int i  -> Match.Int i
    | Match.Param p -> Match.Param {
        p with typ = apply substs p.typ
    }
    | Match.Tuple t -> Match.Tuple (List.map t ~f: (apply_to_pattern substs));
    | Match.List li -> Match.List {
        items = List.map li.items ~f: (apply_to_pattern substs);
        rest = Option.map li.rest ~f: (apply_to_pattern substs);
        item_typ = apply substs li.item_typ;
    }

(* let apply_to_scheme substs scheme = 
    let without_scheme = Type.free_vars scheme |> Set.to_list |> List.fold ~init:substs ~f:Map.remove in
    let typ' = apply without_scheme Type.(scheme.typ) in
    Type.{ typ = typ'; constr } *)

let rec apply_substs_to_params substs = 
    List.map ~f: (fun p -> 
        let typ = apply substs Param.(p.typ) in
        let value = (match p.value with 
            | Param.Optional p -> Param.Optional {p with 
                default = Option.map ~f: (apply_to_expr substs) p.default
            }
            | v -> v
        ) in
        Param.{p with typ; value}
    )

and apply_arg substs = function
    | Apply.PosArg {expr} -> Apply.PosArg{expr = apply_to_expr substs expr}
    | Apply.NameArg {name; expr} -> Apply.NameArg{name; expr = apply_to_expr substs expr}

and apply_ident substs id = 
    Ident.{ id with typ = apply substs id.typ}

and apply_to_expr substs expr = 
    match expr with
    | Expr.Li li -> Expr.Li Li.{ li with
            typ = (apply substs li.typ);
            items = List.map li.items ~f: (apply_to_expr substs)
        }
    | Expr.Foreign n -> Expr.Foreign n (*Foreign.({ n with 
            scheme = Option.map n.scheme ~f: (apply_to_scheme substs)
        })*)
    | Expr.Value m -> Expr.Value ({
            m with typ = apply substs m.typ
        })
    | Expr.Tuple t -> Expr.Tuple Tuple.(
        {t with typ = apply substs t.typ; exprs = List.map t.exprs ~f:(apply_to_expr substs)}
    )
    | Expr.Ident id -> Expr.Ident (
        {id with typ = apply substs id.typ}
    )
    | Expr.Apply m -> Expr.Apply Apply.({m with 
        typ = apply substs m.typ; 
        fn = apply_to_expr substs m.fn; 
        args = List.map m.args ~f:(apply_arg substs)
    })
    | Expr.Lambda lam -> Expr.Lambda Lambda.({ lam with 
        typ = apply substs lam.typ; 
        params = apply_substs_to_params substs lam.params;
        block = apply_to_block substs lam.block
    })
    | Expr.Cond co -> 
        Expr.Cond Cond.({co with 
            typ = apply substs co.typ; 
            cases = List.map co.cases ~f:(fun cas -> {
                if_ = apply_to_block substs cas.if_;
                then_ = apply_to_block substs cas.then_
            }); 
            else_ = Option.map co.else_ ~f:(apply_to_block substs)
        })
    | Expr.Match m -> Expr.Match Match.({ m with
        typ = apply substs m.typ; 
        expr = apply_to_expr substs m.expr;
        cases = List.map m.cases ~f: (fun case -> Match.{ 
            stmts = apply_to_block_stmts substs case.stmts;
            pattern = apply_to_pattern substs case.pattern
        })
    })

and apply_to_block_stmts substs stmts =  
    List.map stmts ~f: (function 
        | Stmt.Block b -> Stmt.Block ({
            b with stmts = apply_to_block_stmts substs b.stmts
        })
        | Stmt.Expr e -> Stmt.Expr (apply_to_expr substs e)
        | Stmt.Let b -> Stmt.Let { b with 
            (* scheme = Option.map b.scheme ~f:(apply_to_scheme substs); *)
            result = apply substs b.result;
            params = apply_substs_to_params substs b.params;
            block = apply_to_block substs b.block
        })

and apply_to_block substs b = {b with stmts = apply_to_block_stmts substs b.stmts}

let apply_to_error substs err = 
    let open Errors in
    match err with
    | Errors.IgnoredResult {unexpected; range} -> 
        Errors.IgnoredResult {unexpected = apply substs unexpected; range}
    | Errors.TypeMismatch { type_expected; type_provided; range} -> 
        Errors.TypeMismatch {
            range;
            type_expected = apply substs type_expected;
            type_provided = apply substs type_provided;
        }
    | Errors.NotFunction { type_provided; range } -> Errors.NotFunction {
        range;
        type_provided = apply substs type_provided;
    }
    | Errors.IfTypeMismatch { unexpected; range } -> Errors.IfTypeMismatch {
        range;
        unexpected = apply substs unexpected;
    }
    | BranchTypeMismatch { unexpected; expected; range } -> Errors.BranchTypeMismatch {
        range;
        unexpected = apply substs unexpected;
        expected = apply substs expected;
    }
    | PatternMismatch t -> PatternMismatch {
        t with 
            expected = apply substs t.expected;
            unexpected = apply substs t.unexpected
    } 
    | ListItemTypeMismatch t -> ListItemTypeMismatch {
        t with 
            expected = apply substs t.expected;
            unexpected = apply substs t.unexpected
    } 
    | UndeclaredIdentifier t -> UndeclaredIdentifier t
    | CyclicDependency t -> CyclicDependency t
    | SourceNotFound t -> SourceNotFound t
    | SourceSymbolNotFound t -> SourceSymbolNotFound t
    | SourceSystemError t -> SourceSystemError t
    | SourceCompileError t -> SourceCompileError t
    | NonExhaustivePatternMatching t -> NonExhaustivePatternMatching t
    | UnusedMatchCase t -> UnusedMatchCase t
    | CannotApplyWithLabel t -> CannotApplyWithLabel{t with lambda = apply substs t.lambda}
    | CannotApplyWithoutLabel t -> CannotApplyWithoutLabel {t with lambda = apply substs t.lambda}
    | NotModule t -> NotModule t