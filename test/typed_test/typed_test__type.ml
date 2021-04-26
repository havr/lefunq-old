open Typed
open Typed.Typed_common
open Typed_infer_helper

let foreign name = Type.make_name (Qualified.just_name name None) Type.Foreign

let tests = [
    "foreign", `Quick, test 
        ~code: {|
            type MyType n = foreign

            let main = foreign "foreign" (MyType -> MyType)
        |}
        ~expect: (Success {
            asserts = [
                assert_let "main" (Type.make_scheme [] (Type.Lambda.make_positional [
                    Type.Simple (foreign "MyType", []); 
                    Type.Simple (foreign "MyType", []); 
                ]))
            ]
        });

    "app", `Quick, test 
        ~code: {|
            type List n = foreign

            let fn = foreign "foreign" (t -> List t)
            let main = fn 10
        |}
        ~expect: (Success {
            asserts = [
                assert_let "main" (Type.make_scheme [] (Type.Lambda.make_positional [
                    Type.Simple (foreign "List", [Type.Simple (foreign "Int", [])]); 
                ]))
            ]
        });
]
