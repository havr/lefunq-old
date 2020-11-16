module TypedTest = struct 
  let test_ok () = 
    Alcotest.(check bool) "ok" true true


  let tests = [
    "lexeme ok", `Quick, test_ok;
  ]
end

let () = Alcotest.run "Parlex" [
  "Parser", TypedTest.tests
]

