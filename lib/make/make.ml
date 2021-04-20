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

(* let absolute_path curr_dir source = 
    if String.is_prefix ~prefix: "./" source then
        curr_dir ^ source
    else if String.is_prefix ~prefix: "../" source then
        curr_dir ^ source
    else source *)

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
    }
    type result = {
        source: string;
        root: Typed.Module.t
    }

    type context = {
        config: config;
        callback: result -> unit;
        mutable sources: Typed.Symbol.Module.t StringMap.t;
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

    let source_path cwd source =
        if String.is_prefix source ~prefix: "/" then
            source
        else if is_relative source then
            (* TODO: realpath *)

            resolve_path (cwd ^ "/" ^ source)
            (*Core.Filename.realpath (cwd ^ source)*)
        else 
            (* TODO: resolve package name *)
            resolve_path(cwd ^ "/" ^ source)
        (* raise (Invalid_argument source) *)

    let ensure_extension str = 
        if String.is_suffix ~suffix: ".le" str 
        then str
        else str ^ ".le"

    let resolve_source_file ~ctx import_stack source =
        let cwd = match import_stack with
            | head :: _ -> Core.Filename.dirname head
            | [] -> ctx.config.fs.cwd ()
        in
        let abs_path = ensure_extension (source_path cwd source) in
        match Fs.(ctx.config.fs.exists) abs_path with
        | false -> None
        | true -> Some abs_path


    type abort_reason = [
        | `NotFound
        | `ReadError
        | `CompileError
    ]

    let convert_typed_error file = function
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
            msg = "If type mismatch: " ^ (Typed.Type.to_string unexpected);
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

    let process ~ctx source =
        let rec resolve_source import_stack source = 
            match resolve_source_file ~ctx import_stack source with
                | None -> Error Typed.Resolve.SourceNotFound
                | Some file_name ->
                    match check_cycles import_stack file_name with
                    | Some loop -> 
                        Error (Typed.Resolve.CyclicDependency loop)
                    | None ->
                        match Map.find ctx.sources file_name with
                        | Some r -> Ok (file_name, Typed.Symbol.Module.(r.exposed))
                        | None ->
                            match compile (file_name :: import_stack) file_name with
                            | Error `NotFound -> Error Typed.Resolve.SourceNotFound
                            | Error `SourceErrors -> Error Typed.Resolve.SourceError
                            | Error _ -> Error Typed.Resolve.SourceNotFound
                            | Ok resolved -> Ok (file_name, Typed.Symbol.Module.(resolved.exposed))
                                (* let scope = Typed.Symbol.Module.{ 
                                    modu = Some resolved; 
                                    binding = None;
                                    typedef = None 
                                } in Ok (file_name, scope) *)
        and compile import_stack file_name = 
                match ctx.config.fs.read file_name with
                | None -> Error `ReadError
                | Some content ->
                    match Ast.of_string ~file:file_name content with
                    | Error error -> 
                        ctx.errors <- error :: ctx.errors;
                        Error `SourceErrors
                    | Ok root -> 
                        match Typed.root ~source:file_name ~resolve_source: (resolve_source import_stack) root with
                        | Error errors ->
                            ctx.errors <- (List.map errors ~f: (convert_typed_error file_name)) @ ctx.errors;
                            Error `SourceErrors
                        | Ok modu ->
                            let source_module = Typed.Symbol.Module.{id = Typed.Symbol.Id.make file_name [] ""; exposed = modu.exposed} in
                            ctx.sources <- Map.add_exn ctx.sources ~key: file_name ~data: source_module;
                            ctx.callback {source=file_name; root = modu};
                            Ok source_module
                
        in match resolve_source_file ~ctx [] source with
            | None -> Error `NotFound
            | Some file_name -> compile [file_name] file_name

    let run ~config ~callback entrypoint =
        let ctx = {
            config; 
            callback;
            sources = Map.empty(module String); 
            errors = []
        } in
        match process ~ctx entrypoint with
        | Error `NotFound -> Error (NotFound entrypoint)
        | Error `ReadError -> Error ReadError
        | Error `SourceErrors -> Error (SourceErrors (List.rev ctx.errors))
        | Ok _ -> Ok ()
end

(* let resolve_file ~ctx prev_imports source =
    match resolve prev_imports source with
    | Ok path -> bundle ~ctx file
    | Error e -> e

and bundle ~ctx file_name =
    match Fs.(ctx.fs.exists) file_name with
    | false -> Result.error (SourceNotFound {name}) (* file doesn't exist *)
    | true -> Result.ok "foo" 


let frontend ~fs entrypoint = (
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
)  *)

(* let js_backend ~fs typed out_file =
    let js_ast = Js.Convert.root_module typed in
    let printer = Js.Ast.Printer.make ~ident: 4 () in
    Js.Ast.Printer.str "const println = (a) => console.log(a);\n" printer; *)

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

    let header main = {|
(function (cache, modules) {
    function require(name) { return cache[name] || get(name); }
    function get(name) {
        var exports = {}, module = {exports: exports};
        const register = modules[name];
        if (!register) {
            /*throw ("Module not found:" + name)*/
            return null;
        }
        register.call(exports, {}, require, module, exports);
        return (cache[name] = module.exports);
    }
    var main = require("|} ^ main ^ {|");
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

    let require_nodes ~foreign_bindings ~source root = 
        let open Typed.Node in 
        let x = ref 0 in
        let convert_var_name _ = 
            let name = "__" ^ (Int.to_string !x) in
            x := !x + 1;
            name
        in
        let collect_global_deps root = 
            let order = ref [] in
            let dups = ref (Set.empty(module String)) in
            let add_dep ?name dep = 
                let skip = (Set.mem !dups dep) || (String.is_empty dep) || (String.equal dep source) in
                if not skip then (
                    let n = match name with 
                        | Some n -> n
                        | None -> convert_var_name dep
                    in
                    order := (dep, n) :: !order; 
                    dups := Set.add !dups dep
                ) in
            let rec block b = block_stmts Block.(b.stmts) 
            and block_stmts = List.iter ~f:(function
                | Stmt.Expr e -> expr e 
                | Stmt.Block bs -> block bs
                | Stmt.Let t -> block t.block
            )

            and expr = function
                | Value _ -> ()
                | Li li -> List.iter li.items ~f:expr
                | Foreign _ ->
                    (* TODO: check it earlier? *)
                    add_dep ~name: (Js.Convert.foreign_require) (Option.value_exn foreign_bindings)
                | Ident id -> 
                    (match (id.resolution @ [id.resolved]) with
                        | [] -> raise (Common.Unreachable)
                        | res :: _ -> add_dep (Option.value_exn res.absolute).source
                    )
                | Apply app -> 
                    expr app.fn;
                    List.iter app.args ~f: (function 
                        | PosArg{expr = e} -> expr e
                        | NameArg{expr = e; _} -> expr e
                    )
                | Lambda lam -> block lam.block
                | Match m -> (* TODO: visit pattern *)
                    (expr m.expr);
                    List.iter m.cases ~f: (fun case -> block_stmts case.stmts)
                | Cond t -> 
                    List.iter t.cases ~f: (fun {if_; then_} ->
                        block if_;
                        block then_;
                    );
                    Option.iter t.else_ ~f:block
                | Tuple t -> List.iter t.exprs ~f:expr
            in
            let rec modu m = 
                List.iter Module.(m.entries) ~f:(function
                | Module.Binding b -> block b.block
                | Module.Import im -> 
                    add_dep im.resolved_source
                | Module.Module m -> modu m
            ) 
            in modu root; !order
        in 
        let deps = collect_global_deps root in
        let call name args = Js.Ast.Apply.{
            fn = Js.Ast.Expr.Ident (Js.Ast.Ident.{value=name});
            args = args;
        } in
        let str value = Js.Ast.Str.{value} in
        let require name = call "require" [Js.Ast.Expr.Str (str name)] in
        let nodes = List.map deps ~f: (fun (name, var_name) -> 
            Js.Ast.Const.expr var_name (Js.Ast.Expr.Apply (require name))) in
        (nodes, deps)

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
        let (requires, mapping) = require_nodes ~foreign_bindings ~source node in
        let ctx = Js.Convert.{
            source=source;
            required_sources = Map.of_alist_exn(module String) mapping;
        } in
        let body = Js.Convert.root_module ~ctx node in
        Js.Ast.Printer.str (module_header source) backend.printer;
        Js.Ast.Prn.stmts (List.map requires ~f: (fun require -> Js.Ast.Block.Const require)) backend.printer;
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

let make ~fs in_ out = 
    let backend = JsBackend.start ~config: JsBackend.{fs} (Frontend.source_path (Fs.(fs.cwd) ()) in_) in 
    match Frontend.run ~config: Frontend.{fs} ~callback: (fun {source; root} ->
        let foreign_source = (Caml.Filename.remove_extension source) ^ ".js" in
        let bindings = match Caml.Sys.file_exists foreign_source with
            | true -> (match File.read foreign_source with 
                | Some contents -> Some (contents, foreign_source)
                | None -> raise Unexpected
            )
            | false -> None
        in
        (* strip path / filename without ext *)
        (* does $filename.js file exist. if so, read it and attach *)
        Common.log[Pp.to_string [Typed.Node.Print_node.modu root]];
        JsBackend.write_source ~bindings backend source root 
    ) in_ with
    | Ok _ ->
        JsBackend.commit backend out;
        Ok ()
    | Error e -> 
        JsBackend.rollback backend;
        Error e