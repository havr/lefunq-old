
(* TODO: name -> const_name, const_block *)
type const = { name: string; const_expr: const_expr }
(* TODO: r/const_expr/expr_block? *)
and const_expr = ConstExpr of expr | ConstBlock of block
and str = { str_value: string }
and num = { num_value: string }
and ident = { ident_value: string }
and unary = { unary_op: string; unary: expr }
and binary = { binary_op: string; binary_left: expr; binary_right: expr}
and apply = { apply_fn: expr; apply_args: expr list}
and expr = Num of num | Str of str | Unary of unary | Binary of binary | Apply of apply | Ident of ident | Lambda of lambda | Block of block
and return = { return_expr: expr }
and block_stmt = ExprStmt of expr | ConstStmt of const | BlockStmt of block | ReturnStmt of return | CondStmt of cond
and block = { block_stmts: block_stmt list }
and lambda = { lambda_args: string list; lambda_block: block }
(* TODO: reusing const_expr for now, but let's switch to expr_or_block or expr_block *)
and cond = { cond_expr: expr; cond_then: block; cond_else: block option}

module Printer = struct
    type t = {buffer: Buffer.t; ident_sample: string; curr_ident: string; mutable newlined: bool}

    let make ?ident () = 
        let ident = match ident with | Some n -> n | None -> 2 in
        let ident_sample = List.init ident (fun _ -> " ") |> (String.concat "") in
        {buffer = Buffer.create 1; ident_sample = ident_sample; curr_ident = ""; newlined = true}

    let value printer = Buffer.contents printer.buffer
    let str s printer = 
        if printer.newlined then begin
            Buffer.add_string printer.buffer printer.curr_ident;
            printer.newlined <- false 
        end;
        Buffer.add_string printer.buffer s

    (* space separated *)
    let separated fns printer = 
        let total = (List.length fns) - 1 in
        List.iteri (fun i fn -> fn printer; if i < total then str " " printer) fns
    let seq ?sep fns printer = 
        let total = (List.length fns) - 1 in
        let sep = Option.value ~default:"" sep in
        List.iteri (fun i fn -> fn printer; if i < total && sep <> "" then str sep printer) fns

    let newline printer = str "\n" printer; printer.newlined <- true

    let line fns = seq[seq[fns]; newline]

    let ident_up fns printer = 
        seq fns {printer with curr_ident = printer.curr_ident ^ printer.ident_sample}
end

let separate sep list = match list with 
   | [] -> []
   | first :: rest -> 
        first :: (rest |> List.map (fun item -> [sep; item]) |> List.flatten)

module Print = struct 
    open Printer
    let num n = str n.num_value 
    let ident n = str n.ident_value
    (* rename printer methods *)
    let str_ n = str ("\"" ^ n.str_value ^ "\"") 
    let rec const n = 
        let const_block = match n.const_expr with
            | ConstExpr m -> expr m
            | ConstBlock m -> block m
        in Printer.separated [
            (str "const");
            (str n.name);
            (str "=");
            const_block 
        ]
    and binary n = separated [
        expr n.binary_left;
        str n.binary_op;
        expr n.binary_right;
    ]
    and expr: expr -> t -> unit = function
        | Lambda n -> lambda n
        | Num n -> num n
        | Ident n -> ident n
        | Str n -> str_ n
        | Binary n -> binary n
        | Block n -> block n
        | Unary n -> 
            separated [
                str n.unary_op;
                expr (n.unary);
            ]
        | Apply n ->
            let fn = match n.apply_fn with
            | Ident id -> ident id
            | _ -> seq [str "("; expr (n.apply_fn); str ")"]
            in seq [
                fn;
                str "(";
                seq ~sep: ", " (List.map expr n.apply_args);
                str ")";
            ]
    and return n = seq ~sep: " " [str "return"; expr n.return_expr]
    and block_stmt stmt =
        begin match stmt with 
        | ExprStmt n -> seq[expr n; str ";"]
        | ConstStmt n -> seq[const n; str ";"]
        | BlockStmt n -> block n
        | ReturnStmt n -> seq[return n; str ";"]
        | CondStmt n -> 
            let if_line = seq [str "if ("; expr n.cond_expr; str ") {"; newline] in
            let then_block = block_stmts n.cond_then in
            let else_block = match n.cond_else with
            | None -> seq[str "}"]
            | Some m -> seq[str "} else {"; newline; block_stmts m; str "}"] 
            in
            seq [if_line; then_block; else_block; newline]
        end;
    and lambda n = 
        let args = n.lambda_args |> List.map(fun arg -> str arg) |> separate (str ", ") in
        let args_tuple = seq[str "("; seq args; str ")"] in
        let args_block = block_stmts (n.lambda_block) in
            seq [args_tuple; str "=>"; str "{"; newline; ident_up [args_block]; str "}"]
    and block_stmts n = 
        ident_up [seq @@ List.map(fun stmt -> line @@ seq [block_stmt stmt]) (n.block_stmts)];
    and block n = seq [
        str "(() => {"; 
        newline;
        block_stmts n;
        str "}) ()";
    ]
end

(* and block = [`Expr of expr | `Const of const] *)

