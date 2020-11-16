let () = Alcotest.run "Parlex" [
  "Applicative", Applicative_test.tests;
  "Matchers", Matchers_test.tests;
]

