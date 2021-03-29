open Common
open Base
open Erro
open Inferno


type coverage = Nothing | Everything | Something of value
and value = 
    | Tuple of coverage list
    | List of {rest: coverage; coverages: list_coverage list}
    | Variant of (string * coverage) list
    | Uncountable of (string list)
and list_coverage = NoListCoverage | ListCoverage of (coverage list)

let rec coverage_to_string = function
    | Nothing -> "<nothing>"
    | Everything -> "_"
    | Something (Tuple coverages) -> 
        List.map coverages ~f: coverage_to_string 
        |> String.concat ~sep:","
        |> Util.Strings.surround "(" ")"
    | Something (Uncountable values) -> 
        String.concat values ~sep:"|"
    | Something (List {rest; coverages}) -> 
        let coverage_strs = List.map coverages ~f: (function
            | NoListCoverage -> "<none>"
            | ListCoverage items ->
                List.map items ~f: coverage_to_string
                |> String.concat ~sep: ";"
                |> Util.Strings.surround "[" "]"
        ) |> String.concat ~sep:"|" in
        let rest_str = (match rest with
            | Nothing -> ""
            | Something _ -> (raise Unreachable) (* rest parameter shouldn't contain "something" *)
            | Everything -> "..rest") 
        in coverage_strs ^ rest_str
    | Something (Variant _) -> "<TODO: variants>"

let rec list_coverage_equals a b = match (a, b) with
    | ListCoverage la, ListCoverage lb -> List.equal coverage_equals la lb
    | NoListCoverage, NoListCoverage -> true
    | _, _ -> false

and coverage_equals a b = match (a, b) with
    | Nothing, Nothing -> true
    | Everything, Everything -> true
    | Something sa, Something sb -> (match (sa, sb) with
        | Tuple ta, Tuple tb -> List.equal coverage_equals ta tb
        | List {coverages = a_coverages; rest = a_rest}, List {coverages = b_coverages; rest = b_rest} -> 
            (List.equal list_coverage_equals a_coverages b_coverages) && (coverage_equals a_rest b_rest)
        | Uncountable ua, Uncountable ub -> List.equal String.equal ua ub
        | _, _ -> false
    )
    | _, _ -> false

let rec pattern_coverage pattern =
    let open Node.Match in
    match pattern with
    | Any -> Everything
    | Unit -> Something (Tuple [])
    | Int i -> Something (Uncountable [i])
    | Str s -> Something (Uncountable [s])
    | Param _ -> Everything
    | Tuple t -> Something (Tuple (List.map ~f: pattern_coverage t))
    | List {items; rest; _} -> 
        let len = List.length items in
        let coverages = (List.init len ~f: (fun _ -> NoListCoverage)) 
            @ [ListCoverage (List.map ~f: pattern_coverage items)] in
        let rest = (match rest with | Some _ -> Everything | None -> Nothing) in
        Something (List {coverages; rest})

let covers_everything list = not @@ List.exists list 
    ~f: (function | Everything -> false | _ -> true)

let rec merge_coverage base new' = 
    match base, new' with 
    | Something s, Nothing -> Something s
    | Nothing, Something s -> Something s
    | Everything, _-> Everything
    | _, Everything -> Everything
    | Something (Uncountable ua), Something (Uncountable ub) ->
        let u' = Set.union (Set.of_list(module String) ua) (Set.of_list(module String) ub)
            |> Set.to_list in
        Something (Uncountable u')
    | Something (List {coverages = base_coverages; rest = base_rest}), Something (List {coverages = new_coverages; rest = new_rest}) ->
        let make_coverage i = function
            | Everything -> ListCoverage (List.init i ~f: (fun _ -> Everything))
            | Nothing -> NoListCoverage
            | Something _ -> (raise Common.Unreachable)
        in
        let rec merge_lists' i = function
        | [], [] -> []
        | [], n :: nrest -> (merge_list_coverage (make_coverage i base_rest) n) :: (merge_lists' (i + 1) ([], nrest))
        | b :: brest, [] -> (merge_list_coverage b (make_coverage i new_rest)) :: (merge_lists' (i + 1) (brest, []))
        | b :: brest, n :: nrest -> (merge_list_coverage b n) :: (merge_lists' (i + 1) (brest, nrest))
        in 
        let coverages = merge_lists' 0 (base_coverages, new_coverages) in
        let rest = merge_coverage base_rest new_rest in
        Something (List {coverages; rest})
    | Something (Tuple basetup), Something (Tuple newtup) ->
        let merged = List.zip_exn basetup newtup |> List.map ~f: (fun (b, n) -> merge_coverage b n) in
        (match covers_everything merged with
            | true -> Everything
            | false -> Something (Tuple merged)
        )
    | _, _ -> Nothing
and merge_list_coverage a b = match (a, b) with
    | ListCoverage a, ListCoverage b -> ListCoverage (List.zip_exn a b |> List.map ~f: (fun (a, b) -> merge_coverage a b))
    | ListCoverage a, NoListCoverage -> ListCoverage a
    | NoListCoverage, ListCoverage b -> ListCoverage b
    | NoListCoverage, NoListCoverage -> NoListCoverage

let rec simplify_coverage = function
    | Nothing -> Nothing
    | Everything -> Everything
    | Something (Tuple coverages) -> 
        let coverages' = List.map coverages ~f: simplify_coverage in
        (match covers_everything coverages' with
        | true -> Everything
        | false -> Something (Tuple coverages'))
    | Something (Uncountable values) -> Something (Uncountable values)
    | Something (List {coverages; rest}) -> 
        let coverages' = List.map coverages ~f: (function 
            | NoListCoverage -> NoListCoverage 
            | ListCoverage c -> ListCoverage (List.map c ~f: simplify_coverage)
        ) in
        (match rest with
        | Something _ -> (raise Common.TODO)
        | Nothing -> Something(List {coverages = coverages'; rest})
        | Everything -> 
            let squashed = List.fold_right coverages' ~init: [] ~f: (fun cov -> function
                | [] -> (match cov with 
                    | NoListCoverage -> [cov]
                    | ListCoverage items -> 
                        (match not @@ List.exists items ~f: (function 
                            | Nothing -> true 
                            | Something _ -> true 
                            | Everything -> false
                        ) with 
                        | true -> []
                        | false -> [cov]))
                | r -> cov :: r) in
            (match squashed with
                | [] -> Everything
                | coverages -> Something (List{coverages; rest}))
        ) 
    | Something (Variant _) -> (raise Common.TODO) 

let check_is_exhaustive matc =
    let errors = ref [] in
    let result = List.fold Node.Match.(matc.cases) ~init: (Nothing) ~f: (fun result case ->
        let covg = pattern_coverage case.pattern in
        let result' = merge_coverage result covg 
            |> simplify_coverage in
        (if coverage_equals result result' then 
            errors := !errors @ [UnusedMatchCase {range = Span.empty_range}]
        );
        result'
    ) in
    (if not @@ covers_everything [result] then 
        (* TODO: calculate missing cases *)
        errors := !errors @ [NonExhaustivePatternMatching {range = Span.empty_range; missing_cases=[]}]
    );
    !errors

let matc ~ctx ~expr ~block_stmts m =
    let open Node in
    let must_unify expect_typ pattern = 
        let pattern_typ = Match.pattern_to_type pattern in
        do_unify ~ctx expect_typ pattern_typ ~error: (
            PatternMismatch {expected = expect_typ; unexpected = pattern_typ; range = Span.empty_range}
        )
    in
    let rec unify_structs = function
    | Match.List li -> 
        let items_unified_error = List.map li.items ~f: (fun item -> 
            let nested_unified = unify_structs item in
            (must_unify li.item_typ item) && nested_unified
        )
        |> List.exists ~f: (fun ok -> not @@ ok) in

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
        | p, t -> ignore @@ must_unify t p);

        (* TODO:  *)
        let case_result = block_stmts ~ctx case.stmts in
        let result' = match result with
        | Type.Unknown -> case_result
        | t -> ignore @@ do_unify ~ctx result t ~error: (BranchTypeMismatch {
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