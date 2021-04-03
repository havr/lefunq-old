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

(* TODO: command *)
exception Unexpected of string

module rec Const: sig 
    type expr = Expr of Expr.t | Block of Block.t
    and t = {name: string; expr: expr}
end = struct 
    type expr = Expr of Expr.t | Block of Block.t
    and t = {name: string; expr: expr}
end
and Str: sig 
    type t = {value: string}
end = struct
    type t = {value: string}
end

and Num: sig 
    type t = {value: string}
end = struct
    type t = {value: string}
end

and Ident: sig 
    type t = {value: string}
    val make: string -> t
end = struct
    type t = {value: string}

    let make value = {value}
    (* let from_ir ir = {value = Typed.(ir.local_name)} *)
end

and Unary: sig 
    type t = { op: string; expr: Expr.t }
end = struct
    type t = { op: string; expr: Expr.t }
end

and Binary: sig 
    type t = { op: string; left: Expr.t; right: Expr.t} 
end = struct
    type t = { op: string; left: Expr.t; right: Expr.t} 
end

and Apply: sig 
    type t = { fn: Expr.t; args: Expr.t list}
end = struct
    type t = { fn: Expr.t; args: Expr.t list}
end
and Lambda: sig 
    type t = {params: string list; block: Block.t}
    val make: string list -> Block.stmt list -> t
end = struct
    type t = {params: string list; block: Block.t}
    let make params stmts = {params; block = Block.{stmts}}
end
and Assign: sig 
    type t = { ident: Ident.t; expr: Expr.t }
    val make: Ident.t -> Expr.t -> t
end = struct
    type t = { ident: Ident.t; expr: Expr.t }
    let make ident expr = {ident; expr}
end
and Block: sig 
    type stmt = Expr of Expr.t | Const of Const.t | Return of Return.t | Cond of Cond.t | Block of Block.t | Assign of Assign.t
    and t = {stmts: stmt list}
    val assign: Assign.t -> stmt
end = struct
    type stmt = Expr of Expr.t | Const of Const.t | Return of Return.t | Cond of Cond.t | Block of Block.t | Assign of Assign.t
    and t = {stmts: stmt list}
    let assign a = Assign a
end

and Index: sig 
    type t = {expr: Expr.t; value: Expr.t}
end = struct 
    type t = {expr: Expr.t; value: Expr.t}
end
and Object: sig 
    type entry = | Kv of (string * Expr.t) | Pun of string
    type t = {
        entries: entry list
    }
end = struct 
    type entry = | Kv of (string * Expr.t) | Pun of string
    type t = {
        entries: entry list
    }
end
and Property: sig 
    type t = {expr: Expr.t; name: string}
end = struct 
    type t = {expr: Expr.t; name: string}
end
and Call: sig 
    type t = {expr: Expr.t; args: Expr.t list}
end = struct 
    type t = {expr: Expr.t; args: Expr.t list}
end

and Expr: sig 
    type t = 
        | Num of Num.t 
        | Str of Str.t 
        | Object of Object.t
        | Unary of Unary.t 
        | Binary of Binary.t 
        | Apply of Apply.t 
        | Ident of Ident.t 
        | Lambda of Lambda.t 
        | Block of Block.t 
        | Parens of Expr.t
        | Li of (Expr.t list)
        | Void
        | Index of Index.t
        | Property of Property.t
        | Call of Call.t

    val ident: Ident.t -> t
end = struct
    type t = 
        | Num of Num.t 
        | Str of Str.t 
        | Object of Object.t
        | Unary of Unary.t 
        | Binary of Binary.t 
        | Apply of Apply.t 
        | Ident of Ident.t 
        | Lambda of Lambda.t 
        | Block of Block.t 
        | Parens of Expr.t
        | Li of (Expr.t list)
        | Void
        | Index of Index.t
        | Property of Property.t
        | Call of Call.t

    let ident id = Ident id
end

and Return: sig 
    type t = {expr: Expr.t}
end = struct
    type t = {expr: Expr.t}
end

and Cond: sig 
    type cond = {if_: Expr.t; then_: Block.stmt list}
    type t = {conds: cond list; else_: Block.stmt list option}
end = struct
    type cond = {if_: Expr.t; then_: Block.stmt list}
    type t = {conds: cond list; else_: Block.stmt list option}
end

module From = struct 
end

let separate sep list = match list with 
   | [] -> []
   | first :: rest -> 
        first :: (rest |> List.map (fun item -> [sep; item]) |> List.flatten)

let (|*|) a b = Printer.seq [a; b]

let join by exprs = 
    let open Base in
    Printer.seq (List.intersperse exprs ~sep: by)

let surround_by_strs start fin expr = Printer.(seq [str start; expr; str fin])
module Prn = struct
    open Printer

    let ident n = str Ident.(n.value)
    let num n = str Num.(n.value)
    let string n = 
        str ("\"" ^ Str.(n.value) ^ "\"") 
    let void = str "(void 0)"

    let rec const n = 
        let const_block = match Const.(n.expr) with
            | Const.Expr m -> expr m
            | Const.Block m -> block m
        in 
        separated [
            (str "const");
            (str n.name);
            (str "=");
            const_block;
            (str ";");
            newline
        ]
    and cond n =
        let ifs = (Cond.(n.conds) |> List.map(fun Cond.{if_; then_} -> 
            seq[str "if ("; expr if_; str ") {"; newline; stmts then_; str "}"]
        )) @ (match n.else_ with
        | Some c -> [seq [str "{"; newline; stmts c; str "}"]]
        | None -> []) in
        seq @@ (separate (str " else ") ifs)
    and stmt = function
        | Block.Assign n -> seq [ident n.ident; str "="; expr n.expr; str ";"]
        | Block.Expr n -> seq[expr n; str ";"]
        | Block.Const n -> seq[const n; str ";"]
        | Block.Block n -> block n
        | Block.Return n -> seq[return n; str ";"]
        | Block.Cond n -> cond n
    and stmts n = 
        ident_up [seq @@ List.map(fun s -> line @@ seq [stmt s]) n];
    and block n = 
        seq [
            str "(() => {"; 
            newline;
            stmts n.stmts;
            str "}) ()"; 
        ] 
    and binary n = separated [
        expr Binary.(n.left);
        str Binary.(n.op);
        expr Binary.(n.right);
    ]
    and objec' obj = 
        let entries = Object.(obj.entries) |> List.map (function
            | Object.Pun k  -> seq [str k]
            | Object.Kv (k, e) -> seq [str k; str ":"; expr e]
        ) |> seq ~sep: ",\n" in
        seq [str "{"; entries; str "}"]
    and expr = function
        | Expr.Index n -> seq [expr Index.(n.expr); str "["; expr n.value; str "]"]
        | Expr.Lambda n -> lambda n
        | Expr.Num n -> num n
        | Expr.Object n -> objec' n
        | Expr.Ident n -> ident n
        | Expr.Void -> void
        | Expr.Str n -> string n
        | Expr.Binary n -> binary n
        | Expr.Block n -> block n
        | Expr.Call n -> 
            let open Base in
            let args = Call.(n.args)
            |> List.map ~f: expr
            |> join (str ", ")
            |> surround_by_strs "(" ")" in
            (expr Call.(n.expr)) |*| args
        | Expr.Property n -> seq [expr Property.(n.expr); str "."; str n.name]
        | Expr.Li li ->
            let open Base in
            let items = List.map ~f:expr li in
            seq[str "["; seq ~sep: "," items; str "]"]
        | Expr.Parens p -> seq[str "("; expr p; str ")"]
        | Expr.Unary n -> 
            separated Unary.[
                str n.op;
                expr n.expr;
            ]
        | Expr.Apply n ->
            let fn = match Apply.(n.fn) with
            | Ident id -> ident id
            | fn -> seq [str "("; expr fn; str ")"]
            in seq [
                fn;
                str "(";
                seq ~sep: ", " (List.map expr n.args);
                str ")";
            ]
    and return n = seq ~sep: " " [str "return"; expr Return.(n.expr)]
    and lambda n = 
        let args = Lambda.(n.params) |> List.map(fun param -> str param) |> separate (str ", ") in
        let args_tuple = seq[str "("; seq args; str ")"] in
        let args_block = stmts Lambda.(n.block.stmts) in
            seq [args_tuple; str "=>"; str "{"; newline; ident_up [args_block]; str "}"]
end

(* TODO: name -> const_name, const_block *)

let const name expr = Const.{name; expr}
let const_block name stmts = Const.{name; expr = Const.Block Block.{stmts}}
let const_expr name expr = Const.{name; expr = Const.Expr expr}
