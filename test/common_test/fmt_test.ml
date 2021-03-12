open Common 

let test_case ~fmt ~args ~expect = (fmt, `Quick, (fun () ->
    let result = fmt %% args in
    Alcotest.(check string) "format match" expect result
))

let tests = [
    test_case ~fmt: "" ~args: [] ~expect: "";
    test_case ~fmt: "no format" ~args: [] ~expect: "no format";
    test_case ~fmt: "no format extra args" ~args: ["1"; "2"] ~expect: "no format extra args";
    test_case ~fmt: "%s" ~args: ["hello"] ~expect: "hello";
    test_case ~fmt: "%s %s" ~args: ["hello"; "world"] ~expect: "hello world";
    test_case ~fmt: "%s text" ~args: ["hello"] ~expect: "hello text";
]