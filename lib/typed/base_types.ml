(* TODO: proper names*)
let int_name = "Int"
let int = Type.Simple (int_name, [])
let str = Type.Simple ("Str", [])
(* TODO: name ()? *)
let unit = Type.Unit 
let bool_name = "Bool"
let bool = Type.Simple (bool_name, [])
let list_name = "List"
let list t = Type.Simple ("List", [t])