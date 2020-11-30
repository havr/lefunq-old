open Base
open Cli.Args
open Cli.Args.Syntax

type t = {src: string}
let args = 
    let+ src = pop in
    {src = src}

let run_node file =
    ignore @@ Caml.Sys.command @@ "node " ^ file


let main args = 
    let tmp_file = Caml.Filename.concat (Caml.Filename.get_temp_dir_name ()) "out" in
    begin match Make.make ~fs:Make.Fs.local args.src tmp_file with
    | Ok _ ->
        Stdio.print_endline tmp_file;
        run_node tmp_file
    | Error es -> 
        List.iter es ~f:(Cli_err.print)
    end