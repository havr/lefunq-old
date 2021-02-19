open Base
open Common

open ANSITerminal

let highlight_position ctx pos =
    let int_length n = Int.to_string n |> String.length in
    let n_lineno_spaces = 1 + (int_length @@ Err.(ctx.start_line) + (List.length ctx.lines)) in
    let pad_lineno n = 
        let no = Int.to_string n in
        let rest = String.make (n_lineno_spaces - String.length no) ' ' in
        no ^ rest ^ "| "
    in
    Err.(ctx.lines) |> List.iteri ~f:(fun i line ->
        let idx = i + ctx.start_line in
        Stdio.print_endline @@ (pad_lineno (idx + 1) ^ line);
        if idx = Pos.(pos.row) - 1 then begin 
            print_string [Foreground Red; Bold] ((String.make (pos.col + n_lineno_spaces + 1) ' ') ^ "^");
            print_string [default; on_default] "\n";
        end
    )

let source_error e = 
    print_string [Bold; Underlined] "Error:";
    print_string [default; on_default] " ";
    Stdio.print_endline @@ Err.(e.msg);
    Stdio.print_endline @@ "";
    begin match e.context with
    | None -> ()
    | Some ctx -> highlight_position ctx e.range.start end;
    Stdio.print_endline @@ "at " ^ (e.file) ^ ":" ^ (Pos.to_string e.range.start) ^ "";
    Stdio.print_endline @@ ""

let print = function
    | Make.NotFound n -> Stdio.print_endline @@ n ^ " not found"
    | Make.ReadError -> Stdio.print_endline @@ "TODO: read error"
    | Make.SourceErrors e -> List.iter e ~f:source_error 