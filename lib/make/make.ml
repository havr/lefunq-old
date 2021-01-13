open Common

module StrKey = struct
    type t = string
    let compare = Stdlib.compare 
end

module FsMap = Map.Make(StrKey)

module Fs = struct
    type t = {
        read: string -> string option;
        write: string -> string -> unit
    }

    let local = let q = {
        read = File.read;
        write = File.write
    } in q

    let mem () = 
        let files = ref (FsMap.empty) in {
            read = (fun filename -> !files |> FsMap.find_opt filename);
            write = (fun filename content -> files := !files |> FsMap.add filename content)
        }
end

exception Todo of string

open Base

let frontend ~fs src_file = 
    let str = Option.value_exn (Fs.(fs.read) src_file) in
    (* let str_lines = String.split ~on:'\n' str in *)
    match Ast.of_string ~file:src_file str with
    | Ok root -> 
        let (typed, errors) = Typed.root root in begin
            match errors with 
            | [] -> Ok typed
            | errors -> 
                errors 
                |> List.map ~f:(fun e -> 
                    (*let line = Typed.Resolve.(e.pos).row in
                    let context_start = if line < 1 then 0 else line - 1 in
                    let length = line - context_start in
                    let context = Err.{
                        lines = List.sub ~pos: context_start ~len: length str_lines;
                        start_line = context_start
                    } in*) Err.{ context = None; file = src_file; pos = Common.Pos.empty;
                        msg = Typed.Error.to_string e})
                |> (fun e -> Error e)
        end
    (* Ok (List.map Typed.Transform.block_stmt block_stmts) *)
    | Error e -> Error [e]
    
let js_backend ~fs typed out_file =
    let js_ast = Js.Convert.root_module typed in
    let printer = Js.Ast.Printer.make ~ident: 4 () in
    Js.Ast.Printer.str "const println = (a) => console.log(a);\n" printer;
    js_ast |> List.iter ~f:(fun node ->
        Js.Ast.Prn.const node printer;
        Js.Ast.Printer.str "\n" printer
    );
    Js.Ast.Printer.str "main();" printer;
    Fs.(fs.write) out_file (Js.Ast.Printer.value printer)

let make ~fs in_ out = 
    match frontend ~fs in_ with
    | Error e -> Error e
    | Ok typed -> 
        js_backend ~fs typed out;
        Ok ()

