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

    let mem () = 
        let files = ref (FsMap.empty) in {
            read = (fun filename -> !files |> FsMap.find_opt filename);
            write = (fun filename content -> files := !files |> FsMap.add filename content);
            exists = (fun filename -> !files |> FsMap.find_opt filename |> Option.is_some);
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
        export: Typed.Symbol.Module.t;
        root: Typed.Module.t
    }

    type context = {
        config: config;
        callback: result -> unit;
        mutable processed: Typed.Symbol.Module.t StringMap.t;
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
    | Typed.Erro.UndeclaredIdentifier { given_name; range } ->
        Common.Err.{
            file; 
            range;
            msg = "Undeclared identifier: " ^ given_name;
            context = None
        }
    | Typed.Erro.TypeMismatch { type_expected; type_provided; range } ->
        Common.Err.{
            file;
            range;
            msg = "Unexpected type: " ^ (Typed.Type.to_string type_provided) ^ " expecting " ^ (Typed.Type.to_string type_expected);
            context = None
        }
    | Typed.Erro.NotFunction { type_provided; range } ->
        Common.Err.{
            file;
            range;
            msg = "Is not a function: " ^ (Typed.Type.to_string type_provided);
            context = None
        }
    | Typed.Erro.IgnoredResult { unexpected; range } ->
        Common.Err.{
            file;
            range;
            msg = "Ignored result: " ^ (Typed.Type.to_string unexpected);
            context = None
        }
    | Typed.Erro.IfTypeMismatch { unexpected; range } ->
        Common.Err.{
            file;
            range;
            msg = "If type mismatch: " ^ (Typed.Type.to_string unexpected);
            context = None
        }
    | Typed.Erro.BranchTypeMismatch { unexpected; expected; range } -> 
        Common.Err.{
            file;
            range;
            msg = "The branch doesn't match. Unexpected " ^ (Typed.Type.to_string unexpected) ^ " expecting " ^ (Typed.Type.to_string expected);
            context = None
        }
    | Typed.Erro.ListItemTypeMismatch { unexpected; expected; range } -> 
        Common.Err.{
            file;
            range;
            msg = "The list has unexpected type " ^ (Typed.Type.to_string unexpected) ^ " expecting " ^ (Typed.Type.to_string expected);
            context = None
        }
        (* TODO:s/list/imports or chains *)
    | Typed.Erro.CyclicDependency { caused_by; list } -> 
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
    | Typed.Erro.SourceNotFound {source} -> 
        Common.Err.{
            file;
            range = source.range;
            msg = "Source not found: " ^ (source.value);
            context = None
        }
    | Typed.Erro.SourceSymbolNotFound {symbol; _} -> 
        Common.Err.{
            file;
            range = symbol.range;
            msg = "Symbol not found: " ^ (symbol.value);
            context = None
        }
    | Typed.Erro.SourceSystemError {source} -> 
        Common.Err.{
            file;
            range = source.range;
            msg = "A system error occured when reading source: " ^ (source.value);
            context = None
        }
    | Typed.Erro.SourceCompileError {source} -> 
        Common.Err.{
            file;
            range = source.range;
            msg = "A source contains errors:" ^ (source.value);
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
                        match Map.find ctx.processed file_name with
                        | Some r -> 
                            let scope = Typed.Resolver.Scope.{ 
                                modu = Some r;
                                binding = None;
                                typedef = None
                            } in
                            Ok (file_name, scope)
                        | None ->
                            match compile (file_name :: import_stack) file_name with
                            | Error `NotFound -> Error Typed.Resolve.SourceNotFound
                            | Error `SourceErrors -> Error Typed.Resolve.SourceError
                            | Error _ -> Error Typed.Resolve.SourceNotFound
                            | Ok resolved -> 
                                let scope = Typed.Resolver.Scope.{ 
                                    modu = Some resolved; 
                                    binding = None;
                                    typedef = None 
                                } in Ok (file_name, scope)
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
                        | Ok (root, def) ->
                            ctx.processed <- Map.add_exn ctx.processed ~key: file_name ~data: def;
                            ctx.callback {source=file_name; root; export = def};
                            Ok def
                
        in match resolve_source_file ~ctx [] source with
            | None -> Error `NotFound
            | Some file_name -> compile [file_name] file_name

    let run ~config ~callback entrypoint =
        let ctx = {
            config; 
            callback;
            processed = Map.empty(module String); 
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
            let rec block b = List.iter Block.(b.stmts) ~f:(function
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
                    (match id.resolved with 
                    | None -> Common.log [id.given_name] 
                    | Some _ -> ());
                    add_dep (Option.value_exn id.resolved).source 
                | Apply app -> 
                    expr app.fn;
                    List.iter app.args ~f:expr
                | Lambda lam -> block lam.block
                | Cond t -> 
                    List.iter t.cases ~f: (fun {if_; then_} ->
                        block if_;
                        block then_;
                    );
                    Option.iter t.else_ ~f:block
                | Tuple t -> List.iter t.exprs ~f:expr
            in
            let modu m = 
                List.iter Module.(m.entries) ~f:(function
                | Module.Binding b -> block b.block
                | Module.Import im -> 
                    add_dep im.resolved_source
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
        let const name expr = 
            Js.Ast.Const.{name; expr = match expr with
            | `Block b -> Js.Ast.Const.Block b
            | `Expr e -> Js.Ast.Const.Expr e}
        in let nodes = List.map deps ~f: (fun (name, var_name) -> const var_name (`Expr (Js.Ast.Expr.Apply (require name)))) in
        (nodes, deps)

    let write_export backend export = 
        Map.iteri Typed.Symbol.Module.(export.bindings) ~f:(fun ~key ~data ->
            (* If module exposes foreign stuff, it should be correctly re-exposed*)
            Js.Ast.Printer.str ("exports." ^ (Js.Convert.ident_value key) ^ " = " ^ Js.Convert.ident_value data.internal.name ^ "\n") backend.printer;
        )

    let write_bindings backend source content = 
        let open Js.Ast.Printer in
        seq ~sep: "\n" [
            str (module_header source);
            str content;
            str module_trailer
        ] backend.printer

    let write_module ~bindings backend source node export =
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
        let body = Typed.Node.Module.(node.entries) |> List.filter_map ~f:(function
            | Typed.Node.Module.Import _ -> None
            | Typed.Node.Module.Binding node -> Some 
                (Js.Convert.let_ ~ctx node)
        ) in
        Js.Ast.Printer.str (module_header source) backend.printer;
        List.iter requires ~f: (fun require -> Js.Ast.Prn.const require backend.printer);
        List.iter body ~f: (fun node -> Js.Ast.Prn.const node backend.printer);
        write_export backend export;
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
    match Frontend.run ~config: Frontend.{fs} ~callback: (fun {source; root; export} ->
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
        JsBackend.write_module ~bindings backend source root export
    ) in_ with
    | Ok _ ->
        JsBackend.commit backend out;
        Ok ()
    | Error e -> 
        JsBackend.rollback backend;
        Error e