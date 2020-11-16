type t = Int of string 
    | Str of string
    | Ident of string 
    | Op of string
    | Whitespace 
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
    (* TODO: ignore them during "iteration" *)
    | Comment

let to_string lexeme = match lexeme with
    | Ident id -> "ident:" ^ id
    | Whitespace -> "whitespace"
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
    | Comment -> "comment"
    | If -> "if"
    | Then -> "then"
    | Else -> "else"

let eof = Eof