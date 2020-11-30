module Const = Const

module Pos = struct 
    type t = {row: int; col: int}
    let to_str pos = (Int.to_string pos.row) ^ ":" ^ (Int.to_string pos.col)
end

module Err = struct 
    type context = {lines: string list; start_line: int}
    type t = {file: string; pos: Pos.t; msg: string; context: context option}
end

module File = struct 
    let read filename = 
        let ch = open_in filename in
        let s = really_input_string ch (in_channel_length ch) in
        close_in ch;
        Some s

    let write filename contents = 
        let out = open_out filename in
        Printf.fprintf out "%s" contents;
        close_out out
end