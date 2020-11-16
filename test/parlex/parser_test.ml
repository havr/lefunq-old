(*
Todo:
  read more about applicatives vs parsers
  try to write expression parser in applicatives (the most complex part)
  parser errors:
   - message
   - position
   - last correct state (recover)
   - is no_match
  think about proper module structure (minimize opens)
  open Parlex (for generic types)
   - Lexeme
   - Pos

   - 't lexeme

  module MyLexer = Parlex.Lexer(Lexeme)
  open MyLexer // only one instance

  module MyParser = Parlex.Parser(Lexeme)
  MyParser.State.t // for state
  MyParser.t // for parser type
  let open MyParser.Syntax in // for let+ syntax
  let open MyParser.Applicative in // for applicatives
 *)