open Base

let (%%) fmt args = 
    let rec split (chunks, curr, idx) = 
        let len = String.length fmt in
        if idx = len then (
            List.rev ((`Text curr) :: chunks)
        ) else if idx = len - 1 then (
            match String.get fmt idx with 
            | '\\' -> 
                split (chunks, curr ^ "%%FORMAT%%", idx + 1) 
            | '%' -> 
                split (chunks, curr ^ "%%FORMAT%%", idx + 1) 
            | c -> split (chunks, curr ^ (Char.to_string c), idx + 1)
        ) else (
            let next = String.sub fmt ~pos: (idx + 1) ~len: 1 in
            match String.get fmt idx with 
            | '\\' -> 
                split (chunks, curr ^ next, idx + 2) 
            | '%' -> (match next with 
                    | "s" -> 
                        split ((`Placeholder) :: (`Text curr) :: chunks, "", idx + 2) 
                    | _ -> 
                        split (chunks, curr ^ "%%FORMAT%%", idx + 2) 
                )
            | c -> split (chunks, curr ^ (Char.to_string c), idx + 1)
        )
    in 
    let rec execute result args = function
    | (`Placeholder) :: rest -> (match args with
        | [] -> execute (result ^ "%%ARG%%") [] rest
        | carg :: rargs -> execute (result ^ carg) rargs rest
    )
    | (`Text t) :: rest -> execute (result ^ t) args rest
    | [] -> result
    in execute "" args (split ([], "", 0))


