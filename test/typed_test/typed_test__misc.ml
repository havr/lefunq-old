open Typed
open Typed.Typed_common
open Typed_infer_helper

let foreign name = Type.make_name (Qualified.just_name name None) Type.Foreign

let tests = [
    "foreign unit", `Quick, test 
        ~code: {|
            let outer fn = {
                let inner a = {
                    fn a
                }
                inner 1
            }
        |}
        ~expect: (Success {
            asserts = [
                assert_let "outer" (Type.make_scheme ["a"] (Type.Lambda.make_positional [
                    Type.Lambda.make_positional [
                        Base_types.int;
                        Type.Var "a";
                    ];
                    Type.Var "a";
                ]))
            ]
        });
]
