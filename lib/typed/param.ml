open Base

type shape = 
| Unit
| Tuple of t list 
| Name of string
and t = { shape: shape; type': Type.t}

type params = t list

let rec names = function
| Tuple ns -> List.map ns ~f:(fun sh -> names sh.shape) |> List.concat
| Name n -> [n]
| Unit -> []

let rec to_string a =
    let type_suffix = match a.type' with 
    | Type.Unknown -> ""
    | t -> ":" ^ Type.to_string t
    in
    let shape_str = match (a.shape) with
    | Unit -> "Unit"
    | Name n -> n
    | Tuple t -> 
        let ts = List.map t ~f:to_string |> String.concat ~sep:" " in
        "(" ^ ts ^ ")"
    in
    shape_str ^ type_suffix

let rec equals a b = 
    let type_equals = Type.equals a.type' b.type' in
    let shape_equals = match (a.shape, b.shape) with
        | Name na, Name nb -> phys_equal na nb
        | Tuple ta, Tuple tb ->
            if List.length ta <> List.length tb then false else
                List.zip_exn ta tb 
                |> List.find ~f:(fun (ela, elb) ->
                    not @@ equals ela elb
                )
                |> Option.is_none
        | _, _ -> false
    in type_equals && shape_equals