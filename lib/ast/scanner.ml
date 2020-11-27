open Parlex.Lexer.Matcher
open Lexeme

let withoutZero = "123456789"
let digit = "0123456789"
let alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let identStart = Base.String.concat ["_"; alpha]
let identMiddle = Base.String.concat ["_"; alpha; digit; "'"]
let identEnd = Base.String.concat ["_"; alpha; digit; "'"; "!"]

let double_quote = char "\""

let foo = []

let skip = [
    (* move "not char" into combinator *)
    seq [str "//"; (many @@ not @@ char "\n")];
    seq [str "/*"; (many @@ not @@ str "*/"); str "*/"];
    oneMore (char " \t");
]

let defs = [
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
        choice [str "+"; str "-"; str "*"; str "/"; str "$"; str "|>"; str ">"; str "<"; str "=="; str "!="]);
    ((fun s -> Ident s), 
        seq [char identStart; many (char identMiddle); maybe (char identEnd)]
    );
]

let config = Parlex.Lexer.Match.{
    lexemes = defs;
    skip = skip;
    unexpected = (fun s -> Error s);
    eof = Eof;
}

let scan_all str = 
    let state = Parlex.Lexer.State.make str in
    Parlex.Lexer.Match.all config state