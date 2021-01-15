open Base

type item = {
    text: string;
    range: Span.range option
}

type branch = {
    items: item list;
    sub: branch list;
}
    
let text text = {text; range = None}

let spanned item = {
    text = Span.(item.value);
    range = Some item.range
}

let branch items sub = { items; sub }

let ident = 2

let rec padding = function
| 0 -> ""
| 1 -> " "
| n -> " " ^ padding (n - 1)

let pad n str = (padding n) ^ str

let to_string ?(print_span=None) branches =
    let span = match print_span with
    | None -> fun _ -> ""
    | Some (`Only_pos) -> fun span -> Pos.to_string Span.(span.start)
    | Some (`Full_span) -> fun span -> (Pos.to_string Span.(span.start)) ^ ":" ^ (Pos.to_string span.end')
    in

    let item n = 
        let span_part = match n.range with
        | None -> ""
        | Some s -> span s
        in
        span_part ^ n.text
    in 
    
    let rec stringify n branch = 
        let item = List.map branch.items ~f:item 
            |> String.concat ~sep: " " |> pad n in 
        let sub = List.map branch.sub ~f: (stringify (n + 1)) in
        String.concat ~sep: "\n" (item :: sub)
    in List.map branches ~f: (stringify 0) |> String.concat ~sep: "\n" 