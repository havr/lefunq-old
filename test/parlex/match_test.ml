open Common
open Base
open Parlex.Lexer

let lexeme = "hello"
let mismatch = "%%%"
let skip = "###"
let eof = "eof"

let config = Match.{
    lexemes = [(fun s -> s), Matcher.str lexeme];
    skip = [Matcher.char "#"];
    unexpected = (fun s -> "unexpected:" ^ s);
    eof = eof
}

let str_lexemes xs = xs |> List.map ~f:(fun lexeme -> Span.(lexeme.value)) |> String.concat ~sep:" "

let unexpected_lexemes xs = 
        Alcotest.fail ("Unexpected lexemes: " ^ (str_lexemes xs))

let eof () = 
    let state = State.make "" in
    let matcher = Match.next config in 
    match matcher state with
    | ([eof], _) -> 
        Alcotest.(check string) "matches" "eof" eof.value
    | (xs, _) -> unexpected_lexemes xs

let mismatch_eof () = 
    let state = State.make mismatch in
    let matcher = Match.next config in 
    match matcher state with
    | ([m; eof], _) -> 
        Alcotest.(check string) "mismatch matches" mismatch m.value;
        Alcotest.(check string) "eof matches" "eof" eof.value
    | (xs, _) -> unexpected_lexemes xs
        
let skip_mismatch_eof () = 
    let state = State.make @@ skip ^ mismatch ^ skip ^ mismatch  in
    let matcher = Match.next config in 
    match matcher state with
    | ([m1; m2; eof], _) -> 
        Alcotest.(check string) "mismatch matches" mismatch m1.value;
        Alcotest.(check string) "mismatch matches" mismatch m2.value;
        Alcotest.(check string) "eof matches" config.eof eof.value
    | (xs, _) -> unexpected_lexemes xs

let skip_mismatch_lexeme () = 
    let state = State.make @@ skip ^ mismatch ^ lexeme in
    let matcher = Match.next config in 
    match matcher state with
    | ([m; hello], _) -> 
        Alcotest.(check string) "mismatch matches" mismatch m.value;
        Alcotest.(check string) "match matches" lexeme hello.value;
    | (xs, _) -> unexpected_lexemes xs

(*
TODO:
  - nexter check
  - expected error
  - compiler modes: string intrapolation
  -  / hex literals / 100_100_100 / floats
  - refactor
    - compare_lexemes (table driven tests)
    - thing something easier for nexter
    - move parlex into separate files as before
 *)

let tests = [
    "eof", `Quick, eof;
    "mismatch_eof", `Quick, mismatch_eof;
    "skip_mismatch_eof", `Quick, skip_mismatch_eof;
    "skip_mismatch_lexeme", `Quick, skip_mismatch_lexeme;
]
