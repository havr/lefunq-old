type t = Int of string 
    | Str of string
    | Ident of string 
    | Op of string
    | Let
    | Eq
    | Semi
    | OpenBlock
    | CloseBlock
    | OpenBracket
    | CloseBracket
    | OpenParen
    | CloseParen
    | LineBreak
    | Colon
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
    | Foreign
    | Module
    | Pipe
    | Spread
    | Match
    | Ampersand
    | Type
    | Error of string

let to_string lexeme = match lexeme with
    | Ident id -> "ident:" ^ id
    | Int id ->  "int:" ^ id
    | Op op -> "op:" ^ op
    | Let -> "let"
    | Str s -> "str:" ^ s
    | Eq -> "="
    | Colon -> ":"
    | Semi -> ";"
    | OpenBlock -> "{"
    | CloseBlock -> "}"
    | LineBreak -> "\\n"
    | Eof -> "eof"
    | OpenBracket -> "["
    | CloseBracket -> "]"
    | OpenParen -> "("
    | CloseParen -> ")"
    | Coma -> ","
    | Lambda -> "\\"
    | If -> "if"
    | Then -> "then"
    | Else -> "else"
    | Rec -> "rec"
    | Import -> "using"
    | Sig -> "sig"
    | FuncArrow -> "->"
    | Foreign -> "foreign"
    | Module -> "module"
    | Pipe -> "|"
    | Match -> "?"
    | Spread -> ".."
    | Ampersand -> "&"
    | Type -> "type"
    | Error e -> "error: " ^ e

let eof = Eof