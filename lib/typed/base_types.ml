open Typed_common

(* TODO: how can we get less hardcoded values *)
let base_name name = Type.make_name (Qualified.just_name name (Some (Id.make "" [] name))) (Type.Foreign)

let int_name = "Int"
let int = Type.Simple (base_name int_name, [])
let str = Type.Simple (base_name "Str", [])
(* TODO: name ()? *)
let unit = Type.Unit 
let bool_name = "Bool"
let bool = Type.Simple (base_name bool_name, [])
let list_name = "List"
let list t = Type.Simple (base_name "List", [t])