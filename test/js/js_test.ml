(* open Js
open Ir *)
open Common
open Base

let run cmds = 
    let inp = Unix.open_process_in (String.concat ~sep:" " cmds) in
    let r = Core.In_channel.input_lines inp in
    Core.In_channel.close inp; 
    r

let execute code = 
    let memfs = Make.Fs.mem () in
    (*TODO: proper path join / split*)
    memfs.write "/./src.lf" code;
    match Make.make ~fs:memfs "./src.lf" "dst.js" with
    | Ok () -> 
        (* TODO: cleanup, unique tmp file name *)
        (* TODO: cleanup *)
        File.write "./tmp.js" (Option.value_exn (memfs.read "dst.js"));
        Stdio.print_endline (Option.value_exn (memfs.read "dst.js"));
        Ok(run ["node"; "./tmp.js"])
    | Error e -> 
        Stdio.print_endline (Option.value ~default: "" (memfs.read "dst.js"));
        Error(e)

module EndToEnd = struct 
    type test = {
        name: string;
        expect: string;
        code: string
    }

    let alcotest case () =
        match execute case.code with
        | Error (Make.NotFound n) -> 
            Alcotest.fail (n ^ " not found")
                |> ignore
        | Error Make.ReadError -> 
            Alcotest.fail ("read error")
                |> ignore
        | Error (Make.SourceErrors es) -> 
            List.map es ~f: (fun e -> Caml.Format.sprintf "%s: %s" (Span.range_str e.range) e.msg)
                |> String.concat ~sep: "\n" 
                |> Alcotest.fail
                |> ignore
        | Ok lines -> 
            let got = String.concat ~sep:"\n" lines in
            if not @@ String.equal got case.expect then begin 
                Alcotest.(fail) ("result mismatch: " ^ got ^ " expected " ^ case.expect)
            end; ()


    let define name tests = [
        let alcodefs = tests |> List.map ~f:(fun test -> 
            (test.name, `Quick, alcotest test)
        ) in Alcotest.run name ["Root", alcodefs]
    ]
end


(* let () = Alcotest.run "Parlex" [
  "Applicative", Applicative_test.tests;
  "Matchers", Matchers_test.tests;
] *)
let () = ignore @@ EndToEnd.define "lambdas" [
    {
        name = "multiple";
        expect = "a\nb\nc";
        code = {|
            // comment
            let fn a = if a > 0 then "a" else if a < 0 then "b" else "c"
            let main () = {
                let a = fn 1
                println a
                let b = fn (-1)
                println b
                let c = fn 0
                println c
            }
        |}
    };
    (* move into "operators" *)
    {
        name = "allows moving |> and $ to the next line";
        expect = "42";
        code = {|
            let (|>) x f = f x 
            let ($) f x = f x
            let add a b = a + b
            let main () = add 2 
                $ 40 
                |> println
        |}
    };
    {
        name = "specifies a custom operator";
        expect = "42";
        code = {|
            let (|>) x f = f x 
            let main () = 42 |> println
        |}
    };
    (* move into "idens" *)
    {
        name = "translates non-ident js symbols";
        expect = "42";
        code = {|
            let __should'run'' = 42
            let main () = println __should'run''
        |}
    };
    (* move into "if" *)
    {
        name = "comparison";
        expect = "more\nless\nequal";
        code = {|
            let if_chain n = if n > 0 then "more" else if n < 0 then "less" else "equal"
            let main () = {
                println (if_chain 1)
                println (if_chain (-1))
                println (if_chain 0)
            }
        |}
    };
    (* move into "exprs" *)
    {
        name = "factorial";
        expect = "720";
        code = {|
            let rec fact n = 
                if n > 1 then 
                    n * (fact (n - 1)) 
                else 
                    1
            let main () = println (fact 6)
        |}
    };

    (* todo: move out into "parens" tests *)
    {
        name = "js is precedence-aware when it puts parens";
        expect = "189";
        code = {|
            let main () = println ((1 + 2) * (3 + 4) * (4 + 5))
        |}
    };
    {
        name = "parses multiple parens correctly";
        expect = "triple_parens";
        code = {|
            let main () = println ((("triple_parens")))
        |}
    };
    (* 123123 *)
    {
        name = "test";
        expect = "hello, world";
        code = {|
            let main () = println "hello, world"
        |}
    };
    {
        name = "lambda with multiple arguments";
        expect = "42";
        code = {|
            let main () = \a b {let c = a + b; println c} 2 40
        |}
    };
    {
        name = "lambda that calls lambda";
        expect = "42";
        code = {|
            let main () = \a b {let c = a b; println c} \a {2 + a} 40 
        |}
    };
    {
        name = "let-fn";
        expect = "42";
        code = {|
            let myfn a b = a + b
            let main () = {let a = myfn 40 2; println a}
        |}
    };
]