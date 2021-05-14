open Base
open Common

type t = 
| UndeclaredIdentifier of {
    range: Span.range;
    given_name: string;
} | TypeMismatch of {
    range: Span.range;
    type_expected: Type.t;
    type_provided: Type.t;
} | NotFunction of {
    range: Span.range;
    type_provided: Type.t
} | NotModule of {
    name: string Span.t
} | IgnoredResult of {
    range: Span.range;
    unexpected: Type.t;
} | IfTypeMismatch of {
    range: Span.range;
    unexpected: Type.t
} | BranchTypeMismatch of {
    range: Span.range;
    unexpected: Type.t;
    expected: Type.t
} | ListItemTypeMismatch of {
    range: Span.range;
    unexpected: Type.t;
    expected: Type.t
} | CyclicDependency of {
    caused_by: string Span.t;
    list: string list;
} | SourceNotFound of {
    source: string Span.t;
} | SourceSymbolNotFound of {
    source: string Span.t;
    symbol: string Span.t;
} | SourceSystemError of {
    source: string Span.t;
} | SourceCompileError of {
    source: string Span.t;
} | PatternMismatch of {
    range: Span.range;
    unexpected: Type.t;
    expected: Type.t;
} | UnusedMatchCase of {
    range: Span.range;
} | NonExhaustivePatternMatching of {
    range: Span.range;
    (* use a data structure *)
    missing_cases: string list;
} | CannotApplyWithLabel of {
    range: Span.range;
    label: string;
    lambda: Type.t;
} | CannotApplyWithoutLabel of {
    range: Span.range;
    lambda: Type.t;
} | InternalError of {
    message: string;
}
 
let clear_range = function 
| UndeclaredIdentifier {given_name; _} -> UndeclaredIdentifier {range = Span.empty_range; given_name}
| TypeMismatch {type_expected = te; type_provided = tp; _} ->
    TypeMismatch {type_expected = te; type_provided = tp; range = Span.empty_range}
| IgnoredResult {unexpected; _} -> 
    IgnoredResult {unexpected; range = Span.empty_range}
| NotFunction {type_provided=tp; _} -> 
    NotFunction {type_provided=tp; range = Span.empty_range}
| NotModule {name} -> 
    NotModule {name = Span.empty name.value}
| IfTypeMismatch {unexpected=u; _} -> 
    IfTypeMismatch {unexpected=u; range = Span.empty_range}
| BranchTypeMismatch {unexpected=u; expected=e; _} -> 
    BranchTypeMismatch {unexpected=u; expected=e; range = Span.empty_range}
| ListItemTypeMismatch {unexpected=u; expected=e; _} -> 
    ListItemTypeMismatch {unexpected=u; expected=e; range = Span.empty_range}
| PatternMismatch {unexpected=u; expected=e; _} -> 
    PatternMismatch {unexpected=u; expected=e; range = Span.empty_range}
| SourceNotFound {source} -> 
    SourceNotFound {source = {source with range = Span.empty_range}}
| SourceSymbolNotFound {source; symbol} -> 
    SourceSymbolNotFound {source = {source with range = Span.empty_range}; symbol}
| CyclicDependency {caused_by; list} -> 
    CyclicDependency {caused_by = {caused_by with range = Span.empty_range}; list}
| SourceCompileError {source} -> 
    SourceCompileError {source = {source with range = Span.empty_range}}
| SourceSystemError {source} -> 
    SourceSystemError {source = {source with range = Span.empty_range}}
| NonExhaustivePatternMatching {missing_cases; _} -> 
    NonExhaustivePatternMatching {range = Span.empty_range; missing_cases}
| UnusedMatchCase _ -> UnusedMatchCase {range = Span.empty_range}
| CannotApplyWithLabel r -> CannotApplyWithLabel {r with range = Span.empty_range}
| CannotApplyWithoutLabel r -> CannotApplyWithoutLabel {r with range = Span.empty_range}
| InternalError i -> InternalError i

let concat = String.concat ~sep: " "
let to_string = function
| UndeclaredIdentifier {given_name; range} ->
    concat ["Undeclared identifier:"; Span.range_str range; given_name]
| TypeMismatch {type_expected = te; type_provided = tp; range} ->
    concat ["Type Mismatch:"; Span.range_str range; Type.to_string te; "!="; Type.to_string tp]
| IgnoredResult {unexpected; range} -> 
    concat ["Ignored result:"; Span.range_str range; Type.to_string unexpected]
| NotFunction {type_provided=tp; range} -> 
    concat ["Not a Function:"; Span.range_str range; Type.to_string tp]
| NotModule {name} -> 
    concat ["Not a Module:"; Span.range_str name.range; name.value]
| IfTypeMismatch {unexpected=u; range} -> 
    concat ["If Type Mismtach:"; Span.range_str range; Type.to_string u]
| BranchTypeMismatch {unexpected=u; expected=e; range} -> 
    concat ["Branch type mismtach:"; Span.range_str range; Type.to_string u; "!="; Type.to_string e]
| PatternMismatch {unexpected=u; expected=e; range} -> 
    concat ["Pattern mismtach:"; Span.range_str range; Type.to_string u; "!="; Type.to_string e]
| ListItemTypeMismatch {unexpected=u; expected=e; range} -> 
    concat ["List item mismtach:"; Span.range_str range; Type.to_string u; "!="; Type.to_string e]
| SourceNotFound {source} -> 
    concat ["Source file not found:"; Span.range_str source.range; source.value]
| SourceSymbolNotFound {source; symbol} -> 
    concat ["Source"; Span.range_str source.range; source.value; "doesn't contain the symbol:"; Span.range_str symbol.range; symbol.value]
| CyclicDependency {caused_by; list} -> 
    concat ["Cyclic dependency";
        Span.range_str caused_by.range;
        caused_by.value;
        "caused py";
        String.concat ~sep:" " list
    ]
| SourceCompileError {source} -> 
    concat ["Source contains errors"; Span.range_str source.range; source.value]
| SourceSystemError {source} -> 
    concat ["System error while reading source"; Span.range_str source.range; source.value]
| UnusedMatchCase {range} -> 
    concat ["Match cases is unused"; Span.range_str range]
| NonExhaustivePatternMatching{range; missing_cases} -> 
    concat ["Non exhaustive pattern matching"; Span.range_str range; String.concat missing_cases ~sep: ", "]
| CannotApplyWithLabel{range; label; lambda} -> 
    concat ["Lambda contains only positional arguments. Cannot apply with label"; Span.range_str range; "Label:"; label; Type.to_string lambda]
| CannotApplyWithoutLabel{range; lambda} -> 
    concat ["Lambda contains only positional arguments. Cannot apply without a label:"; Span.range_str range; Type.to_string lambda]
| InternalError {message} ->
    concat ["Internal error:"; message]

let equals a b = String.equal (to_string a) (to_string b)