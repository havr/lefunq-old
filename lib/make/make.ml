open Common

module StrKey = struct
    type t = string
    let compare = Stdlib.compare 
end

module FsMap = Map.Make(StrKey)

module Fs = struct
    type t = {
        read: string -> string option;
        write: string -> string -> unit;
        exists: string -> bool;
        cwd: unit -> string
    }

    let local = let q = {
        read = File.read;
        write = File.write;
        exists = Sys.file_exists;
        cwd = Caml.Sys.getcwd
    } in q

    let mem files = 
        let open Base in
        let fs = 
            files 
            |> List.fold 
                ~init: (FsMap.empty) 
                ~f:(fun fsmap (name, content) -> FsMap.add name content fsmap) 
            |> ref
        in {
            read = (fun filename -> !fs |> FsMap.find_opt filename);
            write = (fun filename content -> fs := !fs |> FsMap.add filename content);
            exists = (fun filename -> !fs |> FsMap.find_opt filename |> Option.is_some);
            cwd = fun () -> ""
        }
end

exception Todo of string

open Base

type modu = {
    typed: Typed.Node.Module.t
}

type context = {
    fs: Fs.t;
    typed: modu StringMap.t
}

let check_cycles history import =    
    match List.findi history ~f:(fun _ imp -> String.equal imp import) with
    | Some (idx, _) -> Some (List.take history (idx + 1))
    | None -> None

(* TODO: move Common.Err here*)
type error = 
| SourceErrors of (Common.Err.t list)
| ReadError 
| NotFound of string

module Frontend = struct 
    type config = {
        fs: Fs.t;
        packages: (string * string) list;
        builtin: string;
    }
    
    type result = {
        source: string;
        file_path: string;
        root: Typed.Node.Module.t
    }

    type context = {
        config: config;
        callback: result -> unit;
        mutable sources: Typed.Resolved.Module.t StringMap.t;
        mutable errors: Common.Err.t list
    }

    let is_relative name = 
        String.is_prefix name ~prefix: "./" 
        || String.is_prefix name ~prefix: "../"

    (* TODO: what about escaped "/"s *)
    let resolve_path path =
        let result = List.fold (String.split ~on: '/' path) 
            ~init: [] 
            ~f: (fun result -> function
                | "" -> result
                | "." -> result
                | ".." -> List.tl_exn result
                | name -> name :: result
            ) 
            |> List.rev
            |> String.concat ~sep: "/" 
        in "/" ^ result

    let absolute_path cwd source = 
        if String.is_prefix source ~prefix: "/" then
            source
        else resolve_path (cwd ^ "/" ^ source)

    let source_path pkgs cwd source =
        let resolve_absolute abs = 
            let found = List.find_map pkgs ~f: (fun (name, path) -> 
                Common.log[name; path; abs];
                if String.is_prefix abs ~prefix:path then (
                    let pkg_relative = String.chop_prefix_exn abs ~prefix:path in
                    Some (name ^ "/" ^ pkg_relative)
                ) else None
            ) in
            match found with
            | Some rel -> Some (rel, abs)
            | None -> Some (abs, abs)
        in

        if String.is_prefix source ~prefix: "/" then
            resolve_absolute source
        else if is_relative source then
            resolve_absolute (resolve_path (cwd ^ "/" ^ source))
        else 
            List.find_map pkgs ~f: (fun (name, path) ->
                if String.is_prefix source ~prefix: name then (
                    Some (source, resolve_path @@ path ^ "/" ^ (String.chop_prefix_exn source ~prefix: name))
                ) else
                    None
            ) 

    let resolve_source_file ~ctx import_stack source =
        let ensure_extension str = 
            if String.is_suffix ~suffix: ".lf" str 
            then str
            else str ^ ".lf"
        in
        let cwd = match import_stack with
            | head :: _ -> Core.Filename.dirname head
            | [] -> ctx.config.fs.cwd ()
        in
        source_path ctx.config.packages cwd source |> Util.Option.flat_map (fun (rel, abs) ->
            let with_ext = ensure_extension abs in
            let exists = Fs.(ctx.config.fs.exists) in
            if exists with_ext then Some (rel, with_ext) else (
                (* TODO: join it properly: avoid foobar and foo//bar *)
                let index = abs ^ "/index.lf" in
                if exists index then Some (rel, index) else None
            )
        )

    type abort_reason = [
        | `NotFound
        | `ReadError
        | `CompileError
    ]

    let convert_typed_error file = function
    | Typed.Errors.InternalError { message } ->
        Common.Err.{
            file; 
            range = Span.empty_range;
            msg = "Internal error: " ^ message;
            context = None
        }
    | Typed.Errors.UndeclaredIdentifier { given_name; range } ->
        Common.Err.{
            file; 
            range;
            msg = "Undeclared identifier: " ^ given_name;
            context = None
        }
    | Typed.Errors.TypeMismatch { type_expected; type_provided; range } ->
        Common.Err.{
            file;
            range;
            msg = "Unexpected type: " ^ (Typed.Type.to_string type_provided) ^ " expecting " ^ (Typed.Type.to_string type_expected);
            context = None
        }
    | Typed.Errors.PatternMismatch {expected; unexpected; range } ->
        Common.Err.{
            file;
            range;
            msg = "Pattern mismatch: " ^ (Typed.Type.to_string unexpected) ^ " expecting " ^ (Typed.Type.to_string expected);
            context = None
        }
    | Typed.Errors.NotFunction { type_provided; range } ->
        Common.Err.{
            file;
            range;
            msg = "Is not a function: " ^ (Typed.Type.to_string type_provided);
            context = None
        }
    | Typed.Errors.NotModule {name} ->
        Common.Err.{
            file;
            range = name.range;
            msg = "Is not a module: " ^ (name.value);
            context = None
        }
    | Typed.Errors.IgnoredResult { unexpected; range } ->
        Common.Err.{
            file;
            range;
            msg = "Ignored result: " ^ (Typed.Type.to_string unexpected);
            context = None
        }
    | Typed.Errors.IfTypeMismatch { unexpected; range } ->
        Common.Err.{
            file;
            range;
            msg = "If condition expects Bool, but got: " ^ (Typed.Type.to_string unexpected);
            context = None
        }
    | Typed.Errors.BranchTypeMismatch { unexpected; expected; range } -> 
        Common.Err.{
            file;
            range;
            msg = "The branch doesn't match. Unexpected " ^ (Typed.Type.to_string unexpected) ^ " expecting " ^ (Typed.Type.to_string expected);
            context = None
        }
    | Typed.Errors.ListItemTypeMismatch { unexpected; expected; range } -> 
        Common.Err.{
            file;
            range;
            msg = "The list has unexpected type " ^ (Typed.Type.to_string unexpected) ^ " expecting " ^ (Typed.Type.to_string expected);
            context = None
        }
        (* TODO:s/list/imports or chains *)
    | Typed.Errors.CyclicDependency { caused_by; list } -> 
        let rec print_imports = function
        | [] -> raise Common.Unreachable
        | a :: [] -> "\t" ^ a
        | name :: rest -> 
            "\t" ^ name ^ " imports\n" ^ (print_imports rest)
        in
        Common.Err.{
            file;
            range = caused_by.range;
            msg = "Cyclic dependency: " ^ (caused_by.value) ^ " imports\n" ^ (print_imports list);
            context = None
        }
    | Typed.Errors.SourceNotFound {source} -> 
        Common.Err.{
            file;
            range = source.range;
            msg = "Source not found: " ^ (source.value);
            context = None
        }
    | Typed.Errors.SourceSymbolNotFound {symbol; _} -> 
        Common.Err.{
            file;
            range = symbol.range;
            msg = "Symbol not found: " ^ (symbol.value);
            context = None
        }
    | Typed.Errors.SourceSystemError {source} -> 
        Common.Err.{
            file;
            range = source.range;
            msg = "A system error occured when reading source: " ^ (source.value);
            context = None
        }
    | Typed.Errors.SourceCompileError {source} -> 
        Common.Err.{
            file;
            range = source.range;
            msg = "A source contains errors:" ^ (source.value);
            context = None
        }
    | Typed.Errors.UnusedMatchCase {range} -> 
        Common.Err.{
            file;
            range = range;
            msg = "This match case is unused";
            context = None
        }
    | Typed.Errors.NonExhaustivePatternMatching {range; missing_cases} -> 
        Common.Err.{
            file;
            range = range;
            msg = "This match pattern matching is not exhaustive. Unused cases:" ^ (missing_cases |> String.concat ~sep: "\n");
            context = None
        }
    | CannotApplyWithLabel{range; label; lambda} -> 
        Common.Err.{
            file;
            range = range;
            msg = (String.concat ~sep: " " ["Lambda contains only positional arguments. Cannot apply with label"; Span.range_str range; "Label:"; label; Typed.Type.to_string lambda]); 
            context = None
        }
    | CannotApplyWithoutLabel{range; lambda} -> 
        Common.Err.{
            file;
            range = range;
            msg = (String.concat ~sep: " " ["Lambda contains only positional arguments. Cannot apply without a label:"; Span.range_str range; Typed.Type.to_string lambda]);
            context = None
        }

    let rec resolve_source ~builtin ~ctx import_stack source = 
        match resolve_source_file ~ctx import_stack source with
            | None -> Error Typed.Infer_using.SourceNotFound
            | Some (rel, abs) ->
                match check_cycles import_stack rel with
                | Some loop -> 
                    Error (Typed.Infer_using.CyclicDependency loop)
                | None ->
                    match Map.find ctx.sources rel with
                    | Some r -> Ok (rel, Typed.Resolved.Module.(r.exposed))
                    | None ->
                        match compile ~builtin ~ctx (rel :: import_stack) (rel, abs) with
                        | Error `NotFound -> Error Typed.Infer_using.SourceNotFound
                        | Error `SourceErrors -> Error Typed.Infer_using.SourceError
                        | Error _ -> Error Typed.Infer_using.SourceNotFound
                        | Ok resolved -> Ok (rel, Typed.Resolved.Module.(resolved.exposed))
    and compile ~builtin ~ctx import_stack (rel, abs) = 
            match ctx.config.fs.read abs with
            | None -> Error `ReadError
            | Some content ->
                match Ast.of_string ~file:rel content with
                | Error error -> 
                    ctx.errors <- error :: ctx.errors;
                    Error `SourceErrors
                | Ok root -> 
                    match Typed.Infer_toplevel.root ~builtin ~source:rel ~resolve_source: (resolve_source ~builtin ~ctx import_stack) root with
                    | Error errors ->
                        ctx.errors <- (List.map errors ~f: (convert_typed_error rel)) @ ctx.errors;
                        Error `SourceErrors
                    | Ok modu ->
                        let source_module = Typed.Resolved.Module.{id = Typed.Id.make rel [] ""; exposed = modu.exposed} in
                        ctx.sources <- Map.add_exn ctx.sources ~key: rel ~data: source_module;
                        ctx.callback {source = rel; file_path = abs; root = modu};
                        Ok source_module
    let process ~builtin ~ctx source =
        (* TODO: refactor? *)
        let path = absolute_path (ctx.config.fs.cwd ()) source in
        if ctx.config.fs.exists path 
            then compile ~builtin ~ctx [path] (path, path)
            else Error `NotFound

    let map_error ~ctx entrypoint = function
        | Ok v -> Ok v
        | Error `NotFound -> Error (NotFound entrypoint)
        | Error `ReadError -> Error ReadError
        | Error `SourceErrors -> Error (SourceErrors (List.rev ctx.errors))

    let run ~config ~callback entrypoint =
        let ctx = {
            config; 
            callback;
            sources = Map.empty(module String); 
            errors = []
        } in
        let process ~builtin name = 
            match resolve_source_file ~ctx [] name with
            | Some (rel, abs) ->
                Common.log["process name"; name; rel; abs];
                compile ~builtin ~ctx [rel] (rel, abs)
                |> (map_error ~ctx name)
                |> Result.map ~f: (fun m -> Typed.Resolved.Module.(m.exposed))
            | None -> 
                Common.log["process name"; name; "nf"];
                Error (NotFound name)
        in
        let builtin = match config.builtin with
            | "" -> Ok (Map.empty(module String))
            | name -> process ~builtin: (Map.empty(module String)) name
        in
        match builtin with
        | Ok builtin -> (
            process ~builtin entrypoint 
        )
        | Error e -> Error e 
end

module JsBackend = struct 
    module Bundler = struct 

    end
    type config = {
        fs: Fs.t;
    }

    type t = {
        config: config;
        printer: Js.Ast.Printer.t;
    }

(* TODO: need to bundle all node modules there *)

    let header main = {|
(function (cache, modules) {
    function localRequire(name) { return cache[name] || get(name); }
    function get(name) {
        var exports = {}, module = {exports: exports};
        const register = modules[name];
        if (!register) {
            /*throw ("Module not found:" + name)*/
            // TODO: ask for global node module
            return require(name);
            return null;
        }
        register.call(exports, {}, localRequire, module, exports);
        return (cache[name] = module.exports);
    }
    var main = localRequire("|} ^ main ^ {|");
    return main.__esModule ? main.default : main;
})({}, {
|}

    let module_header name = "\"" ^ name ^ "\": (function (global, require, module, exports) {\n"

    let module_trailer = "\n}), "

    let trailer = "});"

    let start main ~config = 
        let printer = Js.Ast.Printer.make ~ident: 4 () in
        Js.Ast.Printer.str "const println = (a) => console.log(a);\n" printer;
        Js.Ast.Printer.str (header main) printer;
        {config; printer}


    let write_bindings backend source content = 
        let open Js.Ast.Printer in
        seq ~sep: "\n" [
            str (module_header source);
            str content;
            str module_trailer
        ] backend.printer

    let write_source ~bindings backend source node =
        let foreign_bindings = (match bindings with
        | Some (content, bindings_source) -> 
            write_bindings backend bindings_source content;
            Some bindings_source
        | None -> None) in
        (* let (requires, mapping) = require_nodes ~foreign_bindings ~source node in
        let ctx = Js.Convert.{
            source=source;
            required_sources = Map.of_alist_exn(module String) mapping;
        } in *)
        let body = Js.Convert.root_module source foreign_bindings node in
        Js.Ast.Printer.str (module_header source) backend.printer;
        (* Js.Ast.Prn.stmts (List.map requires ~f: (fun require -> Js.Ast.Block.Const require)) backend.printer; *)
        Js.Ast.Prn.stmts body backend.printer;
        Js.Ast.Printer.newline backend.printer;
        (* write_exposed backend node.exposed; *)
        Js.Ast.Printer.str module_trailer backend.printer;
        Js.Ast.Printer.str "\n" backend.printer
    
    let commit backend out =  
        Js.Ast.Printer.str trailer backend.printer;
        (* Js.Ast.Printer.str "main();" backend.printer; *)
        Fs.(backend.config.fs.write) out (Js.Ast.Printer.value backend.printer)

    (* clean up tmp files *)
    let rollback backend = ignore backend
end

type config = {
    fs: Fs.t;
    packages: (string * string) list;
    builtin: string;
}

let make ~config in_ out = 
    let cwd = Fs.(config.fs.cwd) () in
    (* TODO: resolve this mess (how to determine main module name before compilation start) *)
    let absname = Frontend.absolute_path cwd in_ in
    let backend = JsBackend.start ~config: JsBackend.{fs = config.fs} absname in 
    let config = Frontend.{
        fs = config.fs;
        packages = config.packages;
        builtin = config.builtin;
    } in
    match Frontend.run ~config ~callback: (fun {source; file_path; root} ->
        (* TODO: what is the best names to call rel/abs source paths? *)
        let foreign_source = (Caml.Filename.remove_extension file_path) ^ ".js" in
        let bindings = match config.fs.exists foreign_source with
            | true -> (match config.fs.read foreign_source with 
                | Some contents -> Some (contents, foreign_source)
                | None -> raise Unexpected
            )
            | false -> None
        in
        (* strip path / filename without ext *)
        (* does $filename.js file exist. if so, read it and attach *)
        JsBackend.write_source ~bindings backend source root 
    ) in_ with
    | Ok _ ->
        JsBackend.commit backend out;
        Ok ()
    | Error e -> 
        JsBackend.rollback backend;
        Error e
