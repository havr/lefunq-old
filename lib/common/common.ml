module Const = Const

exception Unexpected
exception TODO

module Pos = struct 
    type t = {row: int; col: int}
    let to_str pos = (Int.to_string pos.row) ^ ":" ^ (Int.to_string pos.col)
    let empty = {row = 0; col = 0}
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

open Base

let[@inline] dbg args = 
    let print_callee depth = 
        let slots = Caml.Printexc.get_callstack depth |> Caml.Printexc.backtrace_slots in
        match slots with
        | Some slots -> begin
            match Array.get slots (depth - 1) |> Caml.Printexc.Slot.location with
            | Some k -> k.filename ^ ":" ^ (Int.to_string k.line_number)
            | None -> "unknown"
            end
        | None -> "unknown"
    in
    Stdio.print_endline 
        @@ "[" ^ (print_callee 3) ^ "] " ^ (String.concat ~sep:" " args)