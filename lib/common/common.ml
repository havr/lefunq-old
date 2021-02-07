open Base

module Const = Const
(* TODO: remove pp.ml *)
module Pos = Pos
module Span = Span
module Pp = Pp

exception Unexpected
exception Unreachable
exception TODO

module StringMap = Map.M(String)
module StringSet = Set.M(String)

module Err = struct 
    type context = {lines: string list; start_line: int}
    type t = {file: string; range: Span.range; msg: string; context: context option}
end

module File = struct 
    let read filename = 
        let ch = Stdio.In_channel.create filename in
        let s = Caml.really_input_string ch (Stdio.In_channel.length ch |> Int64.to_int_exn) in
        Stdio.In_channel.close ch;
        Some s

    let write filename contents = 
        let out = Stdio.Out_channel.create filename in
        Caml.Printf.fprintf out "%s" contents;
        Stdio.Out_channel.close out
end

let stacktrace ?(depth=5) ?(sep=" => ") () = 
    match Caml.Printexc.get_callstack depth |> Caml.Printexc.backtrace_slots with
    | Some slots ->
        Array.map slots ~f: (fun slot -> match Caml.Printexc.Slot.location slot with
            | Some k -> k.filename ^ ":" ^ (Int.to_string k.line_number)
            | None -> "unknown"
        )
        |> Array.to_list 
        |> List.tl_exn
        |> List.rev
        |> String.concat ~sep
    | None -> "<no stack trace>"

module Trace = struct 
    type t = {
        indent: int;
        enabled: bool;
    }

    let enabled = ref []

    let enable name = enabled := name :: !enabled

    let make name = {
        indent = 0; 
        enabled = List.find !enabled ~f: (String.equal name) |> Option.is_some
    }

    let sub trace = {trace with indent = trace.indent + 2}

    let print ~trace args = 
        if trace.enabled then begin 
            let indent = String.make trace.indent ' ' in
            Stdio.print_endline 
                @@ indent ^ (String.concat ~sep:" " args)
        end
end

let[@inline] log args = 
    let print_callee depth = 
        let slots = Caml.Printexc.get_callstack depth |> Caml.Printexc.backtrace_slots in
        match slots with
        | Some slots -> begin
            match Array.get slots (1) |> Caml.Printexc.Slot.location with
            | Some k -> k.filename ^ ":" ^ (Int.to_string k.line_number)
            | None -> "unknown"
            end
        | None -> "unknown"
    in
    Stdio.print_endline 
        @@ "[" ^ (print_callee 3) ^ "] " ^ (String.concat ~sep:" " args)

module Result_monad = struct
    open Base
    let (let*) = Result.Let_syntax.Let_syntax.bind
end