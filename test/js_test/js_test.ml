(* open Js
open Ir *)
open Common
open Base

let run cmds = 
    let inp = Unix.open_process_in (String.concat ~sep:" " cmds) in
    let r = Core.In_channel.input_lines inp in
    Core.In_channel.close inp; 
    r

let execute files main = 
    let memfs = Make.Fs.mem files in
    (*TODO: proper path join / split*)
    match Make.make ~fs:memfs main "dst.js" with
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
        files: (string * string) list;
        main: string
    }

    let alcotest case () =
        match execute case.files case.main with
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


    let define name tests = 
        let defs = tests |> List.map ~f:(fun test -> 
            (name ^ ":" ^ test.name, `Quick, alcotest test)
        ) in (name, defs)
end

let sigs = EndToEnd.define "sigs" [
    (* {
        name = "working";
        expect = "hello";
        main = "main";
        files = [
            "/main.lf", {|
                sig Str -> Str 
                let f x = x
                let main = {
                    println (f "hello")
                }
            |}
        ]
    } *)
]

let lines = String.concat ~sep: "\n"

(* let () = Alcotest.run "Parlex" [
  "Applicative", Applicative_test.tests;
  "Matchers", Matchers_test.tests;
] *)
let lambdas = EndToEnd.define "lambdas" [
    {
        name = "destruct_tuple_params";
        expect = "-2";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let fn (a, (b, c)) = a * (b - c)
                let main = {
                    println (fn (2, (2, 3)))
                }
            |}
        ]
    };
    {
        name = "apply_immediate_optional_scope";
        expect = "8";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let fn &{a=1} &{b=a+1} &{c=b+2} () = {
                    a * b * c
                }

                let main = {
                    println (fn ())
                }
            |}
        ]
    };
    {
        name = "apply_immediate_optional_2";
        expect = lines ["10"; "100"; "30"];
        main = "main.lf";
        files = [
            "/main.lf", {|
                // comment
                let fn &{a=10} = {
                    println a
                    \ &{b=20} {
                        println b
                        \ &{c=30} () {
                            println c
                        }
                    }
                }

                let main = {
                    fn &b: 100 ()
                }
            |}
        ]
    };
    {
        name = "apply_immediate_optional";
        expect = "-10";
        main = "main.lf";
        files = [
            "/main.lf", {|
                // comment
                let fn &{x=10} a = x - a
                let main = {
                    println (fn 20)
                }
            |}
        ]
    };
    {
        name = "apply_immediate_rewrap";
        expect = "11";
        main = "main.lf";
        files = [
            "/main.lf", {|
                // comment
                let fn &x a &y b = (x - a) + (y - b)
                let fn' = fn 10 &x: 20
                let main = {
                    println (fn' 1 &y: 2)
                }
            |}
        ]
    };
    {
        name = "apply_rewrap_simple";
        expect = "-10";
        main = "main.lf";
        files = [
            "/main.lf", {|
                // comment
                let fn &a b = a - b
                let fn' = fn 20
                let main = {
                    println (fn' &a: 10)
                }
            |}
        ]
    };
    {
        name = "apply_immediate_named2";
        expect = "10";
        main = "main.lf";
        files = [
            "/main.lf", {|
                // comment
                let fn &a b &c d = a + b + c + d
                let main = {
                    println (fn 1 &a:2 3 &c:4)
                }
            |}
        ]
    };
    {
        name = "apply_immediate_named";
        expect = "60";
        main = "main.lf";
        files = [
            "/main.lf", {|
                // comment
                let fn &a b = a + b
                let main = {
                    println (fn (fn 10 &a: 20) &a: 30)
                }
            |}
        ]
    };
    {
        name = "apply";
        expect = "6";
        main = "main.lf";
        files = [
            "/main.lf", {|
                // comment
                let fn a b = a + b
                let main = {
                    println (fn 3 (fn 1 2))
                }
            |}
        ]
    };
    {
        name = "multiple";
        expect = "a\nb\nc";
        main = "main.lf";
        files = [
            "/main.lf", {|
                // comment
                let fn a = if a > 0 then "a" else if a < 0 then "b" else "c"
                let main = {
                    let a = fn 1
                    println a
                    let b = fn (-1)
                    println b
                    let c = fn 0
                    println c
                }
            |}
        ]
    };
    (* move into "operators" *)
    {
        name = "allows moving |> and $ to the next line";
        expect = "42";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let (|>) x f = f x 
                let ($) f x = f x
                let add a b = a + b
                let main = (add 2 40) |> println
                    //(* $ 40  *)
                    //|> println
            |}
        ]
    };
    {
        name = "specifies a custom operator";
        expect = "42";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let (|>) x f = f x 
                let main = 42 |> println
            |}
        ]
    };
    (* move into "idens" *)
    {
        name = "translates non-ident js symbols";
        expect = "42";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let __should'run'' = 42
                let main = println __should'run''
            |}
        ]
    };
    (* move into "if" *)
    {
        name = "comparison";
        expect = "more\nless\nequal";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let if_chain n = if n > 0 then "more" else if n < 0 then "less" else "equal"
                let main = {
                    println (if_chain 1)
                    println (if_chain (-1))
                    println (if_chain 0)
                }
            |}
        ]
    };
    (* move into "exprs" *)
    {
        name = "factorial";
        expect = "720";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let rec fact n = 
                    if n > 1 then 
                        n * (fact (n - 1)) 
                    else 
                        1
                let main = println (fact 6)
            |}
        ]
    };

    (* todo: move out into "parens" tests *)
    {
        name = "js is precedence-aware when it puts parens";
        expect = "189";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let main = println ((1 + 2) * (3 + 4) * (4 + 5))
            |}
        ]
    };
    {
        name = "parses multiple parens correctly";
        expect = "triple_parens";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let main = println ((("triple_parens")))
            |}
        ]
    };
    (* 123123 *)
    {
        name = "test";
        expect = "hello, world";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let main = println "hello, world"
            |}
        ]
    };
    {
        name = "lambda with multiple arguments";
        expect = "42";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let main = \a b {let c = a + b; println c} 2 40
            |}
        ]
    };
    {
        name = "lambda that calls lambda";
        expect = "42";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let main = \a b {let c = a b; println c} \a {2 + a} 40 
            |}
        ]
    };
    {
        name = "let-fn";
        expect = "42";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let myfn a b = a + b
                let main = {let a = myfn 40 2; println a}
            |}
        ]
    };
    (* TODO: move elsewhere into lists *)
    {
        name = "simple lists";
        expect = "[ 1, 2, 3 ]";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let main = println [1; 2; 3]
            |}
        ]
    };
    {
        name = "simple tuples";
        expect = "[ 1, 2, 3 ]";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let main = println (1, 2, 3)
            |}
        ]
    };
]

let matching = EndToEnd.define "js:pattern-match" [
    {
        name = "numbers";
        expect = "42";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let main = {
                    let result = 1 ? {
                        | 1 -> 42
                        | _ -> 100
                    }
                    println result
                }
            |}
        ]
    };

    {
        name = "tuples";
        expect = "10\n20\n30";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let fn x = x ? {
                    | a, 0 -> a
                    | 0, b -> b
                    | a, b -> a + b
                }

                let main = {
                    println (fn (10, 0))
                    println (fn (0, 20))
                    println (fn (10, 20))
                }
            |}
        ]
    };
    {
        name = "lists";
        expect = "0\n1\n-1";
        main = "main.lf";
        files = [
            "/main.lf", {|
                let fn n = n ? {
                    | [] -> 0
                    | [n] -> n
                    | [n ..rest] -> -n
                }

                let main = {
                    println (fn [])
                    println (fn [1])
                    println (fn [1; 2])
                }
            |}
        ]
    }
]

let modules = EndToEnd.define "modules" [
    {
        name = "simple module";
        expect = "42";
        main = "main.lf";
        files = [
            "/main.lf", {|
                module Test = {
                    let answer = 42
                }
                let main = println Test.answer
            |}
        ]
    };
    {
        name = "nested module";
        expect = "42";
        main = "main.lf";
        files = [
            "/main.lf", {|
                module Test = {
                    module Nested = {
                        module Deep = {
                            let answer = 42
                        }
                    }
                }
                let main = println Test.Nested.Deep.answer
            |}
        ]
    };
    {
        name = "shadowed module";
        expect = "42";
        main = "main.lf";
        files = [
            "/main.lf", {|
                module Test = {
                    let answer = 100
                }
                module Test = {
                    let answer = 42
                }
                let main = println Test.answer
            |}
        ]
    };
    {
        name = "shadowed module 2";
        expect = "42";
        main = "main.lf";
        files = [
            "/main.lf", {|
                module Test = {
                    let answer = 42
                }
                module Test = {
                    let answer = 100
                    let answer = Test.answer
                }
                let main = println Test.answer
            |}
        ]
    };
    {
        name = "using modules";
        expect = "42";
        main = "main.lf";
        files = [
            "/other.lf", {|
                module Test = {
                    let answer = 42
                }
            |};
            "/main.lf", {|
                import "./other" { Test }
                let main = println Test.answer
            |}
        ]
    };
    (* language fetaures *)
    (* {
        name = "type";
        expect = "42";
        code = {|
            let a: Int = 40
            let b: Int = 2
            let main = println (a + b)
        |}
    }; *)
]

let using = EndToEnd.define "using" [
    {
        name = "index sources";
        expect = "hello";
        main = "main.lf";
        files = [
            "/other/index.lf", {|
                let hello = "hello"
            |};
            "/main.lf", {|
                import "./other" as Other
                let main = println Other.hello
            |}
        ]
    };
    {
        name = "file-module as module";
        expect = "hello";
        main = "main.lf";
        files = [
            "/other.lf", {|
                let hello = "hello"
            |};
            "/main.lf", {|
                import "./other" as Other
                let main = println Other.hello
            |}
        ]
    };
]

let tests = [sigs; lambdas; modules; matching; using] 