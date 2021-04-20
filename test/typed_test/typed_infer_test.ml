open Typed_infer_helper
open Typed

let tests = [
    "infer:let:simple_application", `Quick, test 
        ~code: {|
            let main f x = f x
        |}
        ~expect: (Success {
            asserts = [
                assert_let "main" (
                    Type.lambda ~constr: ["a"; "b"] [
                        Type.Lambda.make_positional [Type.Var "a"; Type.Var "b"];
                        Type.Var "a";
                        Type.Var "b";
                    ]
                )
            ]
        });
]