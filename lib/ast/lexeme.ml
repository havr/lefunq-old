type t = Int of string 
    | Str of string
    | Ident of string 
    | Op of string
    | Let
    | Eq
    | Semi
    | OpenBlock
    | CloseBlock
    | OpenParen
    | CloseParen
    | LineBreak
    | Coma
    | Lambda
    | Eof
    | If
    | Then
    | Else
    | Rec
    | Import
    | Sig
    | FuncArrow
    | Error of string

let to_string lexeme = match lexeme with
    | Ident id -> "ident:" ^ id
    | Int id ->  "int:" ^ id
    | Op op -> "op:" ^ op
    | Let -> "let"
    | Str s -> "str:" ^ s
    | Eq -> "="
    | Semi -> ";"
    | OpenBlock -> "{"
    | CloseBlock -> "}"
    | LineBreak -> "\\n"
    | Eof -> "eof"
    | OpenParen -> "("
    | CloseParen -> ")"
    | Coma -> ","
    | Lambda -> "\\"
    | If -> "if"
    | Then -> "then"
    | Else -> "else"
    | Rec -> "rec"
    | Import -> "import"
    | Sig -> "sig"
    | FuncArrow -> "->"
    | Error e -> "error: " ^ e

let eof = Eof