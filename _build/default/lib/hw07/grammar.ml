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

open Parsing;;
let _ = parse_error;;
# 2 "lib/hw07/grammar.mly"

(** {0} Grammar rules for parser of MiniML *)

open Util
open Ast
open Lexing

let mk_position s e =
  let start_pos = Parsing.rhs_start_pos s in
  { pos_line = start_pos.pos_lnum;
    pos_col = start_pos.pos_cnum - start_pos.pos_bol;
  } 
    
# 49 "lib/hw07/grammar.ml"
let yytransl_const = [|
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* NOT *);
  263 (* FIX *);
  264 (* PLUS *);
  265 (* MINUS *);
  266 (* DIV *);
  267 (* TIMES *);
  268 (* MOD *);
  269 (* EQ *);
  270 (* NE *);
  271 (* LE *);
  272 (* GE *);
  273 (* LT *);
  274 (* GT *);
  275 (* AND *);
  276 (* OR *);
  277 (* ARROW *);
  278 (* IF *);
  279 (* ELSE *);
  280 (* THEN *);
  281 (* FUN *);
  282 (* LET *);
  283 (* REC *);
  284 (* IN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
  258 (* INTVAL *);
  259 (* BOOLVAL *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\004\000\004\000\004\000\004\000\004\000\
\004\000\005\000\005\000\006\000\006\000\007\000\007\000\007\000\
\008\000\008\000\009\000\009\000\010\000\010\000\011\000\011\000\
\011\000\011\000\011\000\011\000\012\000\012\000\013\000\013\000\
\014\000\014\000\015\000\015\000\016\000\016\000\016\000\002\000\
\002\000\000\000"

let yylen = "\002\000\
\001\000\002\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\001\000\002\000\001\000\003\000\001\000\001\000\001\000\
\001\000\003\000\001\000\001\000\001\000\003\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\001\000\003\000\
\001\000\006\000\001\000\004\000\001\000\007\000\006\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\041\000\004\000\007\000\008\000\000\000\006\000\
\005\000\000\000\000\000\000\000\042\000\001\000\010\000\000\000\
\000\000\000\000\000\000\000\000\033\000\035\000\037\000\040\000\
\000\000\000\000\000\000\000\000\000\000\000\000\011\000\015\000\
\014\000\016\000\000\000\019\000\020\000\000\000\023\000\028\000\
\024\000\025\000\026\000\027\000\000\000\000\000\000\000\009\000\
\000\000\002\000\000\000\000\000\000\000\000\000\000\000\030\000\
\000\000\032\000\000\000\036\000\000\000\000\000\000\000\000\000\
\000\000\034\000\000\000\039\000\038\000"

let yydgoto = "\002\000\
\013\000\014\000\028\000\015\000\016\000\017\000\035\000\018\000\
\038\000\019\000\046\000\020\000\021\000\022\000\023\000\024\000"

let yysindex = "\004\000\
\007\255\000\000\000\000\000\000\000\000\000\000\007\255\000\000\
\000\000\007\255\017\255\001\255\000\000\000\000\000\000\072\255\
\005\255\015\255\051\255\006\255\000\000\000\000\000\000\000\000\
\020\255\003\255\017\255\009\255\017\255\018\255\000\000\000\000\
\000\000\000\000\072\255\000\000\000\000\072\255\000\000\000\000\
\000\000\000\000\000\000\000\000\072\255\072\255\072\255\000\000\
\007\255\000\000\007\255\021\255\007\255\072\255\005\255\000\000\
\015\255\000\000\012\255\000\000\007\255\008\255\007\255\013\255\
\007\255\000\000\007\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\039\000\072\000\094\000\104\000\000\000\000\000\000\000\000\000\
\000\000\000\000\255\254\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\022\000\056\000\000\000\
\088\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\252\255\248\255\240\255\002\000\005\000\000\000\250\255\
\000\000\000\000\000\000\253\255\255\255\000\000\000\000\000\000"

let yytablesize = 388
let yytable = "\031\000\
\012\000\027\000\025\000\030\000\001\000\026\000\003\000\004\000\
\005\000\006\000\007\000\003\000\008\000\009\000\032\000\033\000\
\034\000\027\000\050\000\003\000\052\000\013\000\036\000\037\000\
\048\000\047\000\049\000\029\000\010\000\051\000\053\000\011\000\
\012\000\061\000\063\000\065\000\054\000\031\000\017\000\057\000\
\067\000\056\000\055\000\000\000\059\000\058\000\060\000\000\000\
\062\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\064\000\000\000\066\000\000\000\068\000\000\000\069\000\039\000\
\040\000\041\000\042\000\043\000\044\000\045\000\000\000\021\000\
\004\000\005\000\006\000\007\000\000\000\008\000\009\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\022\000\
\000\000\000\000\000\000\000\000\000\000\029\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\012\000\000\000\000\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\000\000\000\000\012\000\
\012\000\000\000\013\000\000\000\012\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\000\000\017\000\013\000\013\000\017\000\017\000\
\000\000\013\000\000\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\000\000\018\000\017\000\017\000\018\000\
\018\000\000\000\017\000\000\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\021\000\000\000\018\000\018\000\
\000\000\000\000\000\000\018\000\021\000\021\000\021\000\021\000\
\021\000\021\000\021\000\021\000\022\000\000\000\021\000\021\000\
\000\000\000\000\029\000\021\000\022\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\031\000\000\000\022\000\022\000\
\000\000\029\000\000\000\022\000\029\000\029\000\000\000\000\000\
\000\000\029\000\000\000\000\000\000\000\000\000\031\000\031\000\
\000\000\000\000\000\000\031\000"

let yycheck = "\016\000\
\000\000\001\001\007\000\012\000\001\000\010\000\000\001\001\001\
\002\001\003\001\004\001\013\001\006\001\007\001\010\001\011\001\
\012\001\001\001\027\000\021\001\029\000\000\000\008\001\009\001\
\005\001\020\001\024\001\027\001\022\001\021\001\013\001\025\001\
\026\001\013\001\023\001\028\001\035\000\054\000\000\000\046\000\
\028\001\045\000\038\000\255\255\049\000\047\000\051\000\255\255\
\053\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\061\000\255\255\063\000\255\255\065\000\255\255\067\000\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\255\255\000\000\
\001\001\002\001\003\001\004\001\255\255\006\001\007\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\005\001\255\255\255\255\
\008\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\255\255\023\001\
\024\001\255\255\005\001\255\255\028\001\008\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\005\001\023\001\024\001\008\001\009\001\
\255\255\028\001\255\255\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\255\255\005\001\023\001\024\001\008\001\
\009\001\255\255\028\001\255\255\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\005\001\255\255\023\001\024\001\
\255\255\255\255\255\255\028\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\005\001\255\255\023\001\024\001\
\255\255\255\255\005\001\028\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\005\001\255\255\023\001\024\001\
\255\255\020\001\255\255\028\001\023\001\024\001\255\255\255\255\
\255\255\028\001\255\255\255\255\255\255\255\255\023\001\024\001\
\255\255\255\255\255\255\028\001"

let yynames_const = "\
  LPAREN\000\
  RPAREN\000\
  NOT\000\
  FIX\000\
  PLUS\000\
  MINUS\000\
  DIV\000\
  TIMES\000\
  MOD\000\
  EQ\000\
  NE\000\
  LE\000\
  GE\000\
  LT\000\
  GT\000\
  AND\000\
  OR\000\
  ARROW\000\
  IF\000\
  ELSE\000\
  THEN\000\
  FUN\000\
  LET\000\
  REC\000\
  IN\000\
  EOF\000\
  "

let yynames_block = "\
  IDENT\000\
  INTVAL\000\
  BOOLVAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 33 "lib/hw07/grammar.mly"
       ( _1 )
# 287 "lib/hw07/grammar.ml"
               : Ast.term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ident_list) in
    Obj.repr(
# 37 "lib/hw07/grammar.mly"
                   ( _1 :: _2 )
# 295 "lib/hw07/grammar.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "lib/hw07/grammar.mly"
        ( [_1] )
# 302 "lib/hw07/grammar.ml"
               : 'ident_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "lib/hw07/grammar.mly"
        ( Var (_1, mk_position 1 1) )
# 309 "lib/hw07/grammar.ml"
               : 'primary))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "lib/hw07/grammar.mly"
      ( FunConst (Fix, mk_position 1 1) )
# 315 "lib/hw07/grammar.ml"
               : 'primary))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "lib/hw07/grammar.mly"
      ( FunConst (Not, mk_position 1 1) )
# 321 "lib/hw07/grammar.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 45 "lib/hw07/grammar.mly"
         ( IntConst (_1, mk_position 1 1) )
# 328 "lib/hw07/grammar.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 46 "lib/hw07/grammar.mly"
          ( BoolConst (_1, mk_position 1 1) )
# 335 "lib/hw07/grammar.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'term) in
    Obj.repr(
# 47 "lib/hw07/grammar.mly"
                     ( _2 )
# 342 "lib/hw07/grammar.ml"
               : 'primary))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'primary) in
    Obj.repr(
# 51 "lib/hw07/grammar.mly"
          ( _1 )
# 349 "lib/hw07/grammar.ml"
               : 'app_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'app_term) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'primary) in
    Obj.repr(
# 52 "lib/hw07/grammar.mly"
                   ( App (_1, _2, mk_position 1 2) )
# 357 "lib/hw07/grammar.ml"
               : 'app_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'app_term) in
    Obj.repr(
# 56 "lib/hw07/grammar.mly"
           ( _1 )
# 364 "lib/hw07/grammar.ml"
               : 'mult_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mult_op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'app_term) in
    Obj.repr(
# 57 "lib/hw07/grammar.mly"
                             ( BinOp (_2, _1, _3, mk_position 1 3) )
# 373 "lib/hw07/grammar.ml"
               : 'mult_term))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "lib/hw07/grammar.mly"
        ( Mult )
# 379 "lib/hw07/grammar.ml"
               : 'mult_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 62 "lib/hw07/grammar.mly"
      ( Div )
# 385 "lib/hw07/grammar.ml"
               : 'mult_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 63 "lib/hw07/grammar.mly"
      ( Mod )
# 391 "lib/hw07/grammar.ml"
               : 'mult_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mult_term) in
    Obj.repr(
# 67 "lib/hw07/grammar.mly"
            ( _1 )
# 398 "lib/hw07/grammar.ml"
               : 'add_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'add_term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'add_op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_term) in
    Obj.repr(
# 68 "lib/hw07/grammar.mly"
                            ( BinOp (_2, _1, _3, mk_position 1 3) )
# 407 "lib/hw07/grammar.ml"
               : 'add_term))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "lib/hw07/grammar.mly"
       ( Plus )
# 413 "lib/hw07/grammar.ml"
               : 'add_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "lib/hw07/grammar.mly"
        ( Minus )
# 419 "lib/hw07/grammar.ml"
               : 'add_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'add_term) in
    Obj.repr(
# 77 "lib/hw07/grammar.mly"
           ( _1 )
# 426 "lib/hw07/grammar.ml"
               : 'rel_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rel_term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rel_op) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'add_term) in
    Obj.repr(
# 78 "lib/hw07/grammar.mly"
                           ( BinOp (_2, _1, _3, mk_position 1 3) )
# 435 "lib/hw07/grammar.ml"
               : 'rel_term))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "lib/hw07/grammar.mly"
     ( Eq )
# 441 "lib/hw07/grammar.ml"
               : 'rel_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "lib/hw07/grammar.mly"
     ( Le )
# 447 "lib/hw07/grammar.ml"
               : 'rel_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "lib/hw07/grammar.mly"
     ( Ge )
# 453 "lib/hw07/grammar.ml"
               : 'rel_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "lib/hw07/grammar.mly"
     ( Lt )
# 459 "lib/hw07/grammar.ml"
               : 'rel_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "lib/hw07/grammar.mly"
     ( Gt )
# 465 "lib/hw07/grammar.ml"
               : 'rel_op))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "lib/hw07/grammar.mly"
     ( Ne )
# 471 "lib/hw07/grammar.ml"
               : 'rel_op))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rel_term) in
    Obj.repr(
# 91 "lib/hw07/grammar.mly"
           ( _1 )
# 478 "lib/hw07/grammar.ml"
               : 'and_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'rel_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'and_term) in
    Obj.repr(
# 92 "lib/hw07/grammar.mly"
                        ( BinOp (And, _1, _3, mk_position 1 3) )
# 486 "lib/hw07/grammar.ml"
               : 'and_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'and_term) in
    Obj.repr(
# 96 "lib/hw07/grammar.mly"
           ( _1 )
# 493 "lib/hw07/grammar.ml"
               : 'or_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'and_term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_term) in
    Obj.repr(
# 97 "lib/hw07/grammar.mly"
                      ( BinOp (Or, _1, _3, mk_position 1 3) )
# 501 "lib/hw07/grammar.ml"
               : 'or_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_term) in
    Obj.repr(
# 101 "lib/hw07/grammar.mly"
          ( _1 )
# 508 "lib/hw07/grammar.ml"
               : 'ite_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'term) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 102 "lib/hw07/grammar.mly"
                              ( Ite (_2, _4, _6, mk_position 1 6) )
# 517 "lib/hw07/grammar.ml"
               : 'ite_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ite_term) in
    Obj.repr(
# 106 "lib/hw07/grammar.mly"
           ( _1 )
# 524 "lib/hw07/grammar.ml"
               : 'lambda_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'ident_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 107 "lib/hw07/grammar.mly"
                            (
  List.fold_right
    (fun x t -> Lambda (x, t, mk_position 1 4))
    _2 _4
)
# 536 "lib/hw07/grammar.ml"
               : 'lambda_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lambda_term) in
    Obj.repr(
# 115 "lib/hw07/grammar.mly"
              ( _1 )
# 543 "lib/hw07/grammar.ml"
               : 'let_in_term))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'ident_list) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 116 "lib/hw07/grammar.mly"
                                     (
  let fn_rec =
    List.fold_right
      (fun x t -> Lambda (x, t, mk_position 1 7))
      _3 _5
  in
  let fn = App (FunConst (Fix, mk_position 1 7), fn_rec, mk_position 1 7) in
  App (Lambda (List.hd _3, _7, mk_position 1 7), fn, mk_position 1 7)
)
# 560 "lib/hw07/grammar.ml"
               : 'let_in_term))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'ident_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 125 "lib/hw07/grammar.mly"
                                 (
  let fn =
    List.fold_right
      (fun x t -> Lambda (x, t, mk_position 1 6))
      (List.tl _2) _4
  in
  App (Lambda (List.hd _2, _6, mk_position 1 6), fn, mk_position 1 6)
)
# 576 "lib/hw07/grammar.ml"
               : 'let_in_term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'let_in_term) in
    Obj.repr(
# 136 "lib/hw07/grammar.mly"
              ( _1 )
# 583 "lib/hw07/grammar.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "lib/hw07/grammar.mly"
        ( fail (mk_position 1 1) "Syntax error" )
# 589 "lib/hw07/grammar.ml"
               : 'term))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.term)
