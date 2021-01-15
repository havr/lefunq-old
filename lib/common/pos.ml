type t = {row: int; col: int; idx: int}
let to_str pos = (Int.to_string pos.row) ^ ":" ^ (Int.to_string pos.col)
let empty = {row = 0; col = 0; idx = 0}
let equals a b = (a.row = b.row) && (a.col = b.col) && (a.idx = b.idx)

let next char pos = if (char = '\n') then 
    {row = pos.row + 1; col = 1; idx = pos.idx + 1} 
    else {pos with col = pos.col + 1; idx = pos.idx + 1}

let to_string pos = 
    (string_of_int pos.row) ^ ":" ^ (string_of_int pos.col)
