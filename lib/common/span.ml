type range = {
    start: Pos.t;
    end': Pos.t
}

type 't t = {
    range: range;
    value: 't;
}

let from range value = {range; value}

let make start end' value = 
    let range = {start; end'} in
    {range; value}

let empty value = make (Pos.empty) (Pos.empty) value

let merge a b = {start = a.start; end' = b.end'}
let merged a b value = {range = {start = a.start; end' = b.end'}; value}

let equals a b = Pos.equals a.start b.start && Pos.equals a.end' b.end'

let range_str r = 
    if Pos.equals r.start r.end' then
        Pos.to_str r.start
    else
        (Pos.to_str r.start) ^ "-" ^ (Pos.to_str r.end')

let empty_range = {start = Pos.empty; end' = Pos.empty}