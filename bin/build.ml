open Common
open Cli.Args
open Cli.Args.Syntax

type t = {src: string; dst: string}
let args =  
    let+ src = pop
    and+ dst = pop in
    {src = src; dst = dst}

let main args = 
    match Make.make ~fs:Make.Fs.local args.src args.dst with
    | Ok _ -> ()
    | Error e -> 
        print_endline @@ (Pos.to_str e.pos) ^ ": " ^ e.msg