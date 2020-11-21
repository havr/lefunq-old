open Lexeme
open Parlex.Lexer.Matcher

let withoutZero = "123456789"
let digit = "0123456789"
let alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let identStart = Base.String.concat ["_"; alpha]
let identEnd = Base.String.concat ["_"; alpha; digit]

let double_quote = char "\""

let next = Parlex.Lexer.Matcher.next_match [
    ((fun _ -> Comment), 
        (* move "not char" into combinator *)
        seq [str "//"; (many @@ not @@ char "\n")]);
    ((fun _ -> Comment), 
    (* TODO: move this pattern (many not smth) smth into a combinator *)
        seq [str "/*"; (many @@ not @@ str "*/"); str "*/"]);
    ((fun _ -> Let), 
        str "let");
    ((fun _ -> If), 
        str "if");
    ((fun _ -> Then), 
        str "then");
    ((fun _ -> Else), 
        str "else");
    ((fun _ -> LineBreak), 
        oneMore (char "\n"));
    ((fun _ -> Whitespace), 
        oneMore (char " \t"));
    ((fun s -> Int s), 
        seq [oneMore (char digit)]);
    ((fun s -> Str (String.sub s 1 (String.length s - 2))), 
        seq [double_quote; (many @@ not double_quote); double_quote]);
    ((fun _ -> Eq), 
        char "=");
    ((fun _ -> Semi), 
        oneMore @@ char ";");
    ((fun _ -> OpenBlock), 
        char "{");
    ((fun _ -> CloseBlock), 
        char "}");
    ((fun _ -> OpenParen), 
        char "(");
    ((fun _ -> CloseParen), 
        char ")");
    ((fun _ -> Coma), 
        char ",");
    ((fun _ -> Lambda), 
        char "\\");
    ((fun op -> Op op),
        choice [str "+"; str "-"; str "*"; str "/"; str "$"; str "|>"; str ">"; str "<"]);
    ((fun s -> Ident s), 
        seq [char identStart; many (char identEnd)]
    );
]

let keep = function
    | Whitespace -> false
    | Comment -> false
    | _ -> true

let scan_all str = 
    let state = Parlex.Lexer.State.make str in
    let match_all = Parlex.Lexer.Matcher.all next in
    match match_all state with
    | Error e -> Error e
    | Ok lexemes -> 
        lexemes 
            |> List.filter (fun lexeme -> keep Parlex.(lexeme.value))
            |> (fun v -> Ok v)