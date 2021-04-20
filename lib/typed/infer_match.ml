open Common
open Base
open Inferno

module Coverage = struct 
    type t = 
        All 
        | Value of (bool * StringSet.t)
        | Tuple of t list
        | Variant of (string * string list)
        | List of (t list * bool)

    let product ~f a b = Util.Lists.flat_map a ~f:(fun ai ->
        List.map b ~f: (fun bi -> f ai bi)
    )

    let list_from ~rest items = List (items, rest)
    let list_init size ~fill ~rest = list_from ~rest (List.init size ~f: (fun _ -> fill))

    let rec from_pattern pattern =
        let open Node.Match in
        match pattern with
        | Any -> All
        | Unit -> Tuple []
        | Int i -> Value (false, Util.StringSet.from_list [i])
        | Str s -> Value (false, Util.StringSet.from_list [s])
        | Param _ -> All
        | Tuple t -> Tuple (List.map ~f: from_pattern t)
        | List {items; rest; _} -> 
            list_from ~rest: (Option.is_some rest) (List.map items ~f: from_pattern)

    let rec to_string = function 
        | All -> "*"
        | Value (exclude, set) -> 
            let negate = if exclude then "!" else ""in
            negate ^ (String.concat ~sep: "|" (Set.to_list set))
        | Tuple t -> List.map t ~f:to_string 
            |> String.concat ~sep: ", "
            |> Util.Strings.surround "(" ")"
        | List (li, rest) -> 
            List.map li ~f:to_string 
            |> String.concat ~sep: "; "
            |> (fun conc -> conc ^ (if rest then " ..rest" else ""))
            |> Util.Strings.surround "[" "]"
        | Variant (vs, _) -> vs

    let list_to_string xs = List.map ~f:to_string xs |> String.concat ~sep: "; "

    (* TODO: proper equality *)
    let equals a b = String.equal (to_string a) (to_string b)

    let rec invert = function
        | All -> []
        | Value (exclude, set) -> [Value (not exclude, set)]
        | Tuple tup -> (invert_tuple tup)
            |> List.map  ~f: (fun tup' -> Tuple tup')
        | List (li, rest) ->
            let len = List.length li in
            let shorter = List.init len ~f: (fun size -> 
                list_init size ~rest:false ~fill:(All)
            ) in
            let self = List.map (invert_tuple li) ~f: (list_from ~rest) in
            let longer = if rest then [] else [
                list_init ~rest:true ~fill: All (len + 1)
            ] in
            shorter @ self @ longer
        | Variant (v, total) -> 
            total 
            |> List.filter ~f: (fun v' -> not @@ String.equal v' v) 
            |> List.map ~f: (fun v' -> Variant(v', total))
    and invert_tuple = function
        | [] -> []
        | head :: [] -> invert head |> List.map ~f: (fun a -> [a])
        | head :: rest ->
            let heads' = invert head in
            let rests' = invert_tuple rest in
            (product ~f: (fun a b -> a :: b) [head] rests')
                @ (product ~f: (fun a b -> a :: b) heads' [rest])
                @ (product ~f: (fun a b -> a :: b) heads' rests')

    let rec intersect_lists a b = 
        let matched = List.zip_exn a b |> List.map ~f:intersect in
        if List.exists matched ~f: (Option.is_none) 
        then None 
        else Some (List.map ~f: (fun v -> Option.value_exn v) matched)

    and intersect_lists_with_rests a b = 
        let matched = Util.Lists.zipmap_default a b ~default: All ~f:(fun a b -> intersect(a, b)) in
        if List.exists matched ~f: (Option.is_none) 
        then None 
        else Some (List.map ~f: (fun v -> Option.value_exn v) matched)

    and intersect = function 
        | s, All -> Some s
        | All, s -> Some s
        | Value (a_exclude, a_set), Value (b_exclude, b_set) ->
            let include_set s = if Set.length s = 0 then None else Some (Value (false, s)) in
            (match (a_exclude, b_exclude) with
            | true, true ->
                Some (Value (true, Set.union a_set b_set))
            | false, false ->
                include_set (Set.inter a_set b_set)
            | true, false -> 
                include_set (Set.diff b_set a_set)
            | false, true -> 
                include_set (Set.diff a_set b_set))
        | Tuple a, Tuple b ->
            Option.map (intersect_lists a b) ~f: (fun v -> Tuple v)
        | Variant (a, set), Variant (b, _) ->
            if String.equal a b then Some (Variant (a,set)) else None
        | List (a, arest), List (b, brest) ->
            let al = List.length a in
            let bl = List.length b in
            (match (arest, brest) with 
                | false, false -> 
                    if al = bl 
                    then Option.map (intersect_lists a b) ~f: (fun v -> List(v, false))
                    else None
                | true, true ->
                    Option.map (intersect_lists_with_rests a b) ~f: (fun v -> List (v, true))
                | true, false ->
                    if al <= bl then (
                        Option.map (intersect_lists_with_rests a b) ~f: (fun v -> List (v, false))
                    ) else None
                | false, true ->
                    if bl <= al then (
                        Option.map (intersect_lists_with_rests a b) ~f: (fun v -> List (v, false))
                    ) else None
            )
        | _, _ -> None

    let merge os ns =
        product ns os ~f: (fun bi ni ->
            intersect (bi, ni)
        ) 
        |> List.filter_map ~f: (fun v -> v) 
end

let check_is_exhaustive matc =
    let errors = ref [] in
    let result = Option.value_exn (List.fold Node.Match.(matc.cases) ~init: (None) ~f: (fun result case ->
        let coverage = Coverage.from_pattern case.pattern in
        let uncovered = Coverage.invert coverage in
        match result with 
        | None -> Some uncovered 
        | Some result -> 
            ( match Coverage.merge result [coverage] with
            | [] -> 
                errors := !errors @ [Errors.UnusedMatchCase {range = Span.empty_range}];
                Some result
            | _ -> (
                let result' = Coverage.merge result uncovered in
                Some result'
            ))
    )) in
    (match result with 
    | [] -> ()
    | _ -> 
        (* TODO: calculate missing cases *)
        errors := !errors @ [Errors.NonExhaustivePatternMatching {range = Span.empty_range; missing_cases=[]}]
    );
    !errors

let matc ~ctx ~expr ~block_stmts m =
    let open Node in
    let must_unify expect_typ pattern = 
        let pattern_typ = Match.pattern_to_type pattern in
        do_unify ~ctx expect_typ pattern_typ ~error: (
            Errors.PatternMismatch {expected = expect_typ; unexpected = pattern_typ; range = Span.empty_range}
        )
    in
    let rec unify_structs = function
    | Match.List li -> 
        let items_unified_error = List.map li.items ~f: (fun item -> 
            let nested_unified = unify_structs item in
            (must_unify li.item_typ item) && nested_unified
        )
        |> List.exists ~f: (fun ok -> not ok) in

        let rest_unified_error = Option.map li.rest ~f: (fun rest -> 
            let nested_unified = unify_structs rest in
            (must_unify (Base_types.list (li.item_typ)) rest) && nested_unified
        ) |> Option.exists ~f: (fun ok -> not @@ ok) in

        (not items_unified_error) && (not rest_unified_error)
    | Match.Tuple tu -> 
        List.map tu ~f: unify_structs 
        |> List.exists ~f: (fun ok -> not @@ ok)
        |> not
    | _ -> true
    in

    let typ = expr ~ctx Node.Match.(m.expr) in
    (* TODO: assert list types *)
    let (result, patterns_are_ok) = List.fold m.cases ~init: (Type.Unknown, true) ~f: (fun (result, patterns_are_ok) case ->
        let patterns_are_ok' = (unify_structs case.pattern) && patterns_are_ok in
        (match (case.pattern, typ) with
        | Match.Any, _ -> ()
        | p, t -> 
            ignore @@ must_unify t p);

        (* TODO:  *)
        let case_result = block_stmts ~ctx case.stmts in
        let result' = match result with
        | Type.Unknown -> case_result
        | t -> ignore @@ do_unify ~ctx result t ~error: (Errors.BranchTypeMismatch {
                range = Stmt.range (List.last_exn case.stmts);
                expected = result;
                unexpected = t;
            });
            t
        in
        (result', patterns_are_ok')
    ) in 
    (if patterns_are_ok then ctx.errors <- ctx.errors @ (check_is_exhaustive m));
    (* TODO: check if all branches work properly *)
    result