let tests = Alcotest.run "Compiler" (
    Common_test.tests
    @ Ast_test.tests
    @ Typed_test.tests
    @ Js_test.tests
    @ []
)