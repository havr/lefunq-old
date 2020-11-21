(* open Js
open Ir *)
open Common
(* open Base *)

let run cmds = 
    let inp = Unix.open_process_in (String.concat " " cmds) in
    let r = Core.In_channel.input_lines inp in
    Core.In_channel.close inp; 
    r

let execute code = 
    let memfs = Make.Fs.mem () in
    memfs.write "src.lf" code;
    match Make.make ~fs:memfs "src.lf" "dst.js" with
    | Ok () -> 
        (* TODO: cleanup, unique tmp file name *)
        (* TODO: cleanup *)
        File.write "./tmp.js" (memfs.read "dst.js" |> Option.get);
        print_endline (memfs.read "dst.js" |> Option.get);
        Ok(run ["node"; "./tmp.js"])
    | Error e -> 
        print_endline (memfs.read "dst.js" |> Option.value ~default: "");
        Error(e)

module EndToEnd = struct 
    type test = {
        name: string;
        expect: string;
        code: string
    }

    let alcotest case () =
        match execute case.code with
        | Error e -> 
            Alcotest.fail (Format.sprintf "failed at %s: %s" (Pos.to_str e.pos) e.msg)
        | Ok lines -> 
            let got = String.concat "\n" lines in
            if got <> case.expect then begin 
                Alcotest.(fail) ("result mismatch: " ^ got ^ " expected " ^ case.expect)
            end


    let define name tests = [
        let alcodefs = tests |> List.map (fun test -> 
            (test.name, `Quick, alcotest test)
        ) in Alcotest.run name ["Root", alcodefs]
    ]
end


(* let () = Alcotest.run "Parlex" [
  "Applicative", Applicative_test.tests;
  "Matchers", Matchers_test.tests;
] *)

let () = ignore @@ EndToEnd.define "lambdas" [
    (* move to cond *)
    (* {
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
    }; *)
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
            let fact n = 
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
        expect = "helloworld";
        code = {|
            let main () = \a b {let c = a + b; println c} "hello" "world"
        |}
    };
    {
        name = "lambda that calls lambda";
        expect = "helloworld";
        code = {|
            let main () = \a b {let c = a b; println c} \a {"hello" + a} "world"
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