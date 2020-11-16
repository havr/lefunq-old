open Common
open Cli.Args
open Cli.Args.Syntax

type t = {src: string}
let args = 
    let+ src = pop in
    {src = src}

let run_node file =
    ignore @@ Sys.command @@ "node " ^ file

let main args = 
    let tmp_file = Filename.concat (Filename.get_temp_dir_name ()) "out" in
    begin match Make.make ~fs:Make.Fs.local args.src tmp_file with
    | Ok _ ->
        print_endline tmp_file;
        run_node tmp_file
    | Error e -> 
        print_endline @@ (Pos.to_str e.pos) ^ ": " ^ Err.(e.msg)
    end;