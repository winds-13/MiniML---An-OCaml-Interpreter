type token =
  | IDENT of (string)
  | INTVAL of (int)
  | BOOLVAL of (bool)
  | LPAREN
  | RPAREN
  | NOT
  | FIX
  | PLUS
  | MINUS
  | DIV
  | TIMES
  | MOD
  | EQ
  | NE
  | LE
  | GE
  | LT
  | GT
  | AND
  | OR
  | ARROW
  | IF
  | ELSE
  | THEN
  | FUN
  | LET
  | REC
  | IN
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.term
