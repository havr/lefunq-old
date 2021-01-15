type range = {
    start: Pos.t;
    end': Pos.t
}

type 't t = {
    range: range;
    value: 't;
}

let make start end' value = 
    let range = {start; end'} in
    {range; value}

let empty value = make (Pos.empty) (Pos.empty) value

let merge a b = {start = a.start; end' = b.end'}
