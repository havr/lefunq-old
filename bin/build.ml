open Base
open Cli.Args
open Cli.Args.Syntax

type t = {src: string; dst: string}
let args =  
    let+ src = pop
    and+ dst = pop in
    {src = src; dst = dst}

let main args = 
    let config = Make.{
        fs = Make.Fs.local;
        packages = ["std", Sys.getenv_exn "LF_STDLIB"];
        builtin = "std/base"
    } in

    match Make.make ~config:config args.src args.dst with
    | Ok _ -> ()
    | Error e -> Cli_err.print e