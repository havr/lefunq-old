open Base

include Fmt

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
module IntMap = Map.M(Int)

module Err = struct 
    type context = {lines: string list; start_line: int}
    type t = {file: string; range: Span.range; msg: string; context: context option}
    let to_string e = "%s [%s]: %s" %% [e.file; Span.range_str e.range; e.msg]
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

let try_map list ~f =
    let rec loop result = function
    | [] -> Ok (List.rev result)
    | item :: rest ->
        match f item with
        | Ok mapped -> loop (mapped :: result) rest
        | Error e -> Error e
    in loop [] list

let try_fold list ~init ~f =
    let rec loop acc = function
    | [] -> Ok acc
    | item :: rest ->
        match f acc item with
        | Ok acc' -> loop acc' rest
        | Error e -> Error e
    in loop init list

module Util = struct 
    module StringSet = struct 
        let from_list = List.fold 
            ~init:(Set.empty(module String))
            ~f: (fun set v -> Set.add set v)
    end
    module Option = struct 
        let flat_map fn = function
            | Some v -> fn v 
            | None -> None
    end
    module Strings = struct 
        let surround start fin str = start ^ str ^ fin
    end
    module Lists = struct 
        let rec last = function
            | [] -> (raise (Invalid_argument "calling 'last' on an empty list"))
            | [only] -> only
            | _ :: rest -> last rest

        let pick ~f list = 
            let rec pick' head = function
                | [] -> None
                | h :: rest -> (match f h with
                    | true -> Some (h, (List.rev head) @ rest)
                    | false -> pick' (h :: head) rest
                )
            in pick' [] list

        let last_rest l = match List.rev l with
        | [] -> raise (Invalid_argument "list is empty")
        | last :: rest -> (last, List.rev rest)

        let map2 ~f a b = 
            let a', b' = List.zip_exn a b 
                |> List.fold ~init: ([], []) ~f: (fun (ar, br) (a, b) -> 
                    let (a', b') = f (a, b) in (a' :: ar, b' :: br)) 
            in (List.rev a', List.rev b')
            
        let flat_map ~f = List.fold ~init: [] ~f: (fun acc item -> acc @ (f item))
        let flatten = List.fold ~init: [] ~f: (fun acc item -> acc @ item)
        let zipmap_default x y ~f ~default = 
            let rec loop' result x y = match (x, y) with
                | [], [] -> result
                | x :: xs, [] ->
                    loop' (f x default :: result) xs []
                | [], y :: ys -> 
                    loop' (f default y :: result) [] ys
                | x :: xs, y :: ys -> 
                    loop' (f x y :: result) xs ys
            in List.rev (loop' [] x y)
    end
end

let (--) i j = 
    let rec aux n acc =
      if n > j  then acc else aux (n + 1) (n :: acc)
    in aux i [] ;;
