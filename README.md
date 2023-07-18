We consider a core language built around the untyped lambda
calculus. For convenience, we extend the basic lambda calculus with
primitive operations on Booleans and integers. We also introduce a fixpoint
operator to ease the implementation of recursive functions. The
concrete syntax of the language is described by the following grammar:

```
(Variables)
x: var

(Integer constants)
i: int  ::= 
   ... | -1 | 0 | 1 | ...

(Boolean constants)
b: bool ::= true | false

(inbuilt functions)
f: inbuilt_fun ::=
     not                   (logical negation)
   | fix                   (fixpoint operator)

(Binary infix operators)
bop: binop ::=
     * | / | mod           (multiplicative operators)
   | + | - |               (additive operators)
   | && | ||               (logical operators)
   | = | <>                ((dis)equality operators)
   | < | > | <= | >=       (comparison operators)

(Terms)
t: term ::= 
     f                     (inbuilt functions)
   | i                     (integer constants)
   | b                     (Boolean constants)
   | x                     (variables) 
   | t1 t2                 (function application)
   | t1 bop t2             (binary infix operators)
   | if t1 then t2 else t3 (conditionals)
   | fun x -> t1           (lambda abstraction)
```

The rules for operator precedence and associativity are the
same [as in OCaml](https://caml.inria.fr/pub/docs/manual-ocaml/expr.html).

For notational convenience, we also allow OCaml's basic `let`
bindings, which we introduce as syntactic sugar. That is, the OCaml
expression

```ocaml
let x = t1 in t2
```

is syntactic sugar for the following term in our core calculus:

```ocaml
(fun x -> t2) t1
```

Similarly, the OCaml expression

```ocaml
let rec x = t1 in t2
```

is syntactic sugar for the term

```ocaml
(fun x -> t2) (fix (fun x -> t1))
```

We represent the core calculus of MiniML using algebraic data types
as follows:

```ocaml
(** source code position, line:column *)
type pos = { pos_line: int; pos_col: int }

(** variables *)
type var = string

(** inbuilt functions *)
type inbuilt_fun =
  | Fix (* fix (fixpoint operator) *)
  | Not (* not *)

(** binary infix operators *)
type binop =
  | Mult  (* * *)
  | Div   (* / *)
  | Mod   (* mod *)
  | Plus  (* + *)
  | Minus (* - *)
  | And   (* && *)
  | Or    (* || *)
  | Eq    (* = *)
  | Lt    (* < *)
  | Gt    (* > *)
  | Le    (* <= *)
  | Ge    (* >= *)

(** terms *)
type term =
  | FunConst of inbuilt_fun * pos      (* f (inbuilt function) *)
  | IntConst of int * pos              (* i (int constant) *)
  | BoolConst of bool * pos            (* b (bool constant) *)
  | Var of var * pos                   (* x (variable) *)
  | App of term * term * pos           (* t1 t2 (function application) *)
  | BinOp of binop * term * term * pos (* t1 bop t2 (binary infix operator) *)
  | Ite of term * term * term * pos    (* if t1 then t2 else t3 (conditional) *)
  | Lambda of var * term * pos         (* fun x -> t1 (lambda abstraction) *)
```

Note that the mapping between the various syntactic constructs and the
variants of the type `term` is fairly direct. The only additional
complexity in our implementation is that we tag every term with a
value of type `pos`, which indicates the source code position where
that term starts in the textual representation of the term given as
input to our interpreter. We will use this information for error
reporting.

### Code Structure, Compiling and Editing the Code, Running the Interpreter

The code template contains various OCaml modules that already
implement most of the functionality needed for our interpreter:

* [lib/hw07/util.ml](lib/hw07/util.ml): some useful utility
  functions (the type `pos` is defined here)

* [lib/hw07/ast.ml](lib/hw07/ast.ml): definition of abstract syntax
  of MiniML (see above) and related utility functions

* [lib/hw07/grammar.mly](lib/hw07/grammar.mly): grammar definition
  for a parser that parses a MiniML term and converts it into an
  abstract syntax tree of type `term`

* [lib/hw07/lexer.mll](lib/hw07/lexer.mll): associated grammar
  definitions for lexer phase of parser

* [lib/hw07/parser.ml](lib/hw07/parser.ml): interface to MiniML
  parser generated from grammar

* [lib/hw07/eval.ml](lib/hw07/eval.ml): the actual MiniML interpreter

* [bin/miniml.ml](bin/miniml.ml): the main entry point of
  our interpreter application (parses command line parameters and the
  input file, runs the input program and outputs the result, error
  reporting)

* [test/hw07_spec.ml](test/hw07_spec.ml): module with unit tests for
  your code.


The directory structure is configured for the OCaml build tool `dune`.

You can find some test inputs for the interpreter in the directory `tests`.
To compile and run the executable program on a test input, execute e.g. the following command in the root directory of the repository:
```bash
dune exec -- bin/miniml.exe tests/test01.ml
```

The interpreter supports the option `-v` which you can use to get some
additional debug output. In particular, the interpreter will
print the input program on standard output after it has been parsed.

Note that the interpreter will initially fail with an error message
`"Not yet implement"` for each unit test.

To run the unit tests, simply execute
```bash
dune runtest
```

To provide editing support for OCaml in your IDE, we use 
the [Merlin toolkit](https://github.com/ocaml/merlin). Assuming
you have set up an editor or IDE with Merlin, you should be able to
just open the source code files and start editing. Merlin should
automatically highlight syntax and type errors in your code and
provide other useful functionality. 

**Important**: Execute `dune build` once immediately after cloning the
repository. This is needed so that Merlin is able to resolve the
dependencies between the different source code files.
