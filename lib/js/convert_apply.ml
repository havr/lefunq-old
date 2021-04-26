open Base
open Typed

let make_tempvar prefix = 
    let n = ref 0 in
    (fun () ->
        let s = "$" ^ prefix ^ "_" ^ (Int.to_string @@ !n + 1) in
        n := !n + 1;
        s
    )

type arg = {
    pos: int;
    expr: Expr.t;
}

module Shape = struct 
    let from_apply apply = 
        let rec from_arg result = function
        | Type.Lambda (param, to') ->
            let arg = (match param with
                | Type.PosParam _ -> ("", false, `None)
                | Type.NamedParam p -> (p.param_name, p.is_optional, `None) 
                | Type.NamedBlock _ -> (raise Common.TODO) (* Unroll this syntactic sugar before *)
            ) in from_arg (arg :: result) to'
        | _ -> List.rev result
        in 
        let shape' = from_arg [] Typed.Node.(Expr.typ Apply.(apply.fn)) in
        let labeled = List.mapi apply.args ~f: (fun i -> function
            | Typed.Apply.PosArg {expr} -> (i, "", expr)
            | Typed.Apply.NameArg {expr; name} -> (i, name.value, expr)
        ) in
        let inlineable = function
            | Typed.Node.Expr.Value _
            | Typed.Node.Expr.Ident _ -> true
            | _ -> false
        in
        let rec set shape arg = 
            let (pos, name, expr) = arg in
            match shape with
            | (label, eraseable, action) :: rest -> 
                let empty = match action with | `Erase | `None -> true | _ -> false in
                let matches = (String.equal label name) && (empty) in
                let erases = eraseable && (String.is_empty name)in
                if matches 
                then (
                    let act = if inlineable expr 
                        then (`Inline (pos, expr)) 
                        else (`Arg pos) in
                    [label, false, act] @ rest
                ) else if erases then [label, false, `Erase] @ (set rest arg)
                else [label, eraseable, action] @ (set rest arg)
            | [] -> []
        in
        let shape = List.fold labeled ~init: shape' ~f: set
            |> List.map ~f:(fun (_, _, action) -> action) in
        let used = List.fold shape ~init: (Set.empty(module Int)) ~f: (fun m -> function | `Arg i -> Set.add m i | _ -> m) in
        let non_inlined = List.filter_map labeled ~f: (fun (pos, _, expr) ->
            if Set.mem used pos then Some {pos; expr} else None
        ) in
        shape, non_inlined

end


let print_shape shape = 
    let idx_str = function
        | `Arg n -> Int.to_string n
        | `Erase -> "<erase>"
        | `None -> "<none>"
        | `Inline _ -> "<inline>"
    in
    Common.log [
        List.map shape ~f: (fun n -> (idx_str n)) |> String.concat ~sep: "->"
    ]

module Instant = struct 
    type chunk = {
        args: arg list;
        actions: [`Arg of int | `Erase | `Inline of (int * Typed.Expr.t)] list
    }

    let from_actions acts args =
        let rec chunk actions = 
            let erase_inline, rest = List.split_while actions ~f: (function | `Erase -> true | _ -> false) in
            match rest with
                | [] -> (match erase_inline with 
                    | [] -> [] 
                    | erase_inline -> [-1, erase_inline]
                )
                | head :: rest -> 
                    let head_pos = match head with |`Arg n -> n | `Inline (n, _) -> n | _ -> (raise Common.Unreachable) in
                    let to_apply, to_rest = List.split_while rest ~f: (function
                        | `Inline _ -> true
                        | `Erase -> true 
                        | `Arg pos -> pos <= head_pos 
                    ) in
                    (head_pos, erase_inline @ [head] @ to_apply) :: (chunk to_rest)
        in 
        let chunks = chunk acts in
        List.fold_map chunks ~init: args ~f: (fun args (max_pos, actions) ->
            let required, rest = List.split_while args ~f: (fun arg -> arg.pos <= max_pos) in
            rest, {args = required; actions}
        )
end


module Rewrap = struct 
    type t = {
        args: arg list;
        numParams: int;
        actions: [`Arg of int | `Param of int | `Inline of Typed.Expr.t] list;
    }

    let from_actions actions' args = 
        let actions = List.rev actions'
        |> List.drop_while ~f: (function | `None -> true | _ -> false)
        |> List.rev in
        let numParams, actions = List.fold_map actions ~init: 0 ~f:(fun i -> function
            | `Erase | `None -> (i + 1, `Param i)
            | `Arg n -> i, `Arg n
            | `Inline (_, n) -> i, `Inline n
        ) in {args; numParams; actions}
end

type apply = {
    instant: Instant.chunk list option;
    rewrap: Rewrap.t option
}

let split_instant_rewrap actions = 
    let rec collect_erase result = function
        | [] -> None
        | `None :: _ -> None
        | `Erase :: rest -> collect_erase (result @ [`Erase]) rest
        | `Inline _ as a ::rest -> Some (result @ [a], rest)
        | `Arg _ as a :: rest -> Some (result @ [a], rest)
    in let rec cut_chunks result actions = 
        (match collect_erase [] actions with
            | Some (actions, rest) -> cut_chunks (result @ actions) rest
            | None -> result, actions
        )
    in cut_chunks [] actions


let apply app =
    let shape, args = Shape.from_apply app in
    let instant', rewrap = split_instant_rewrap shape in
    let instant = List.map instant' ~f: (function | `Inline i -> `Inline i | `Arg i -> `Arg i | `Erase -> `Erase | _ -> (raise Common.Unreachable)) in
    let rest, instant = Instant.from_actions instant args in
    let rewr = Rewrap.from_actions rewrap rest in
    let instant_opt = match instant with
        | [] -> None
        | chunks -> Some chunks
    in
    let rewr_opt = match Rewrap.(rewr.numParams) with
        | 0 -> None
        | _ -> Some rewr
    in instant_opt, rewr_opt

let convert  ~expr app accessor = 
    let immediate ~tempvar ~arg_vars chunks target =
        let apply_fn target chunk = Ast_util.Apply.curried target @@ List.map Instant.(chunk.actions) ~f: (function
            | `Arg n -> Ast.Expr.Ident (Ast.Ident.make (Map.find_exn arg_vars n))
            | `Inline (_, e) -> expr e
            | `Erase -> Ast.Expr.Null
        ) in
        List.fold chunks ~init: ([], target) ~f: (fun (result, target) chunk ->
            match Instant.(chunk.args) with
            | [] -> 
                (result, apply_fn target chunk)
            | inlineable :: [] ->
                let target = Ast_util.Apply.curried target (List.map chunk.actions ~f: (function
                    | `Inline (_, e) -> expr e
                    | `Arg n -> 
                        if (n = inlineable.pos) 
                        then expr inlineable.expr
                        else Ast.Expr.Ident (Ast.Ident.make (Map.find_exn arg_vars n))
                    | `Erase ->  Ast.Expr.Null
                )) in (result, target)
            | rest -> (
                let (result, target) = match result with
                | [] -> result, target
                | some ->
                    let tv = tempvar () in
                    let result' = some @ [Ast.Block.Const (Ast.Const.expr tv target)] in
                    (* TODO: proper temp param *)
                    let target' = Ast.Expr.Ident (Ast.Ident.make tv) in
                    result', target'
                in
                let consts = List.map rest ~f: (fun {pos; expr = e; _} ->
                    (* TODO proper temp param*)
                    Ast.Block.Const (Ast.Const.expr (Map.find_exn arg_vars pos) (expr e))
                ) in
                (result @ consts, apply_fn target chunk)
            )
        )
    in
    let rewrap ~tempvar ~arg_vars rewr fn = 
        let consts = List.map Rewrap.(rewr.args) ~f: (fun {pos; expr = e; _} ->
            Ast.Block.Const (Ast.Const.expr (Map.find_exn arg_vars pos) (expr e))
        ) in

        let params = List.init Rewrap.(rewr.numParams) ~f: (fun i -> i, tempvar()) in
        let param_vars = List.fold params ~init: (Map.empty(module Int)) ~f: (fun m (param, var) ->
            Map.add_exn m ~key: param ~data: var
         ) in
        let inner_args = List.map Rewrap.(rewr.actions) ~f: (function
            | `Arg i -> Ast.Expr.Ident (Ast.Ident.make (Map.find_exn arg_vars i))
            | `Inline e -> expr e
            | `Param p -> Ast.Expr.Ident (Ast.Ident.make (Map.find_exn param_vars p))
        ) in
        let lambda_params = List.map params ~f: (fun (_, var) -> var) in

        let rewrapped = Ast_util.Lambda.curried lambda_params (
            Ast_util.Apply.curried fn inner_args
        ) in
        consts, rewrapped
    in
    let (instant, rewr) = apply app in
    (* Option.iter instant ~f:(fun chs -> List.iter chs ~f: (fun sh -> print_shape Instant.(sh.actions))); *)
    let tempvar = make_tempvar "" in
    let arg_vars =
        let from_chunks = Option.value ~default: [] @@ Option.map instant ~f: (fun chunks ->
            Util.Lists.flat_map chunks ~f: (fun Instant.{args; _} -> args)) in
        let from_rewr = Option.value ~default: [] @@ Option.map rewr ~f: (fun Rewrap.{args; _} -> args) in
            List.fold (from_chunks @ from_rewr) ~init:(Map.empty(module Int)) ~f: (fun m {pos; _} -> Map.add_exn m ~key: pos ~data: (tempvar())) 
    in
    let result, acc = match instant with
        | None -> [], accessor
        | Some im -> immediate ~tempvar ~arg_vars im accessor 
    in
    let result, acc = match rewr with
        | None -> result, acc
        | Some rewr -> 
            let v = tempvar() in
            let rewr_acc = Ast_util.Const.expr v acc in
            let acc = Ast.Ident.make v in
            let consts', acc' = rewrap ~tempvar ~arg_vars rewr (Ast.Expr.Ident acc) in
            (result @ [rewr_acc] @ consts', acc')
    in
    match result with
    | [] -> acc
    | result ->
        let result' = result @ [Ast.Block.Return (Ast.Return.{expr=acc})] in
        Ast_util.Lambdas.scoped [] (result')