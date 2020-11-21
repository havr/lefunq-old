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

let frontend ~fs src_file = 
    let str = Fs.(fs.read) src_file |> Option.get in
    match Ast.of_string ~file:src_file str with
    | Ok block_stmts -> Ok (List.map Typed.Transform.block_stmt block_stmts)
    | Error e -> Error e
    
let js_backend ~fs typed out_file =
    let js_ast = List.map Js.Convert.block_stmt typed in
    let printer = Js.Ast.Printer.make ~ident: 4 () in
    Js.Ast.Printer.str "const println = (a) => console.log(a);\n" printer;
    js_ast |> List.iter (fun node ->
        Js.Ast.Prn.stmt node printer;
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

