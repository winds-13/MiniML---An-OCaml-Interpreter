(** {0} The evaluation function of the MiniML interpreter. *)

open Ast
open Util

(** Generated a type error at position [pos] for expected type [exp_typ] and actual type [act_typ]. *)
let type_error pos exp_typ act_typ =
  fail pos (Printf.sprintf "Type mismatch\n  Expected type: %s\n  Actual type: %s" exp_typ act_typ)

(** Extract bool from a BoolVal. Generate type error message if value is not a BoolVal *)
let bool_of_value pos = function
  | BoolVal b -> b
  | IntVal i -> type_error pos "bool" "int"
  | Closure _ -> type_error pos "bool" "function"

(** Extract int from an IntVal. Generate type error message if value is not an IntVal *)
let int_of_value pos = function
  | IntVal i -> i
  | BoolVal _ -> type_error pos "int" "bool"
  | Closure _ -> type_error pos "int" "function"

(** Extract closure components from a Closure value. Generate type error message if value is not a Closure *)
let closure_of_value pos = function
  | Closure (x, t, env) -> x, t, env
  | IntVal _ -> type_error pos "function" "int"
  | BoolVal _ -> type_error pos "function" "bool"

(** Convert a value back to a term *)
let term_of_value pos = function
  | IntVal i -> IntConst (i, pos)
  | BoolVal b -> BoolConst (b, pos)
  | Closure (x, t, _) -> Lambda (x, t, pos)

(** Evaluate term t to a value using call-by-value semantics *)
let eval beta (t: term) : value =
  let rec eval t = match t with
    | FunConst (Fix, pos) ->
      let f = Var ("f", pos) in
      let x = Var ("x", pos) in
      let fn =
        Lambda ("x",
                App (App (f, App (t, f, pos), pos),
                     x, pos),
                pos)
      in
      Closure ("f", fn, [])
    | FunConst (Not, pos) ->
      let x = Var ("x", pos) in
      let fn = Lambda ("x", App (t, x, pos), pos) in
      Closure ("x", fn, [])
    | IntConst (i, _) -> IntVal i
    | BoolConst (b, _) -> BoolVal b
    | App (FunConst (Not, _), t, pos) ->
      (* TODO: implement evaluation of Boolean negation *)
      BoolVal (not (bool_of_value pos (eval t)))  
    | App (t1, t2, pos) ->
      (* Evaluate function application. The actual work is delegated to the function beta. *)
      beta t1 t2 pos
    | BinOp (bop, t1, t2, pos) -> 
      (* TODO: implement evaluation of binary operators.
       * Use pattern matching on bop to differentiate between the different operators.
       * Make sure to implement short-circuiting semantics for And and Or. *)
      (match bop with
       | Mult -> IntVal (( * ) (int_of_value pos (eval t1)) (int_of_value pos (eval t2)))  (* * *)
       | Div -> IntVal ((/) (int_of_value pos (eval t1)) (int_of_value pos (eval t2))) (* / *)
       | Mod -> IntVal ((mod) (int_of_value pos (eval t1)) (int_of_value pos (eval t2))) (* mod *)
       | Plus -> IntVal ((+) (int_of_value pos (eval t1)) (int_of_value pos (eval t2))) (* + *)
       | Minus -> IntVal ((-) (int_of_value pos (eval t1)) (int_of_value pos (eval t2)))(* - *)
       | Eq -> (match t1 with 
           | BoolConst (b1, b2) -> BoolVal ((=) (bool_of_value pos (eval t1)) (bool_of_value pos (eval t2)))
           | _ -> BoolVal ((=) (int_of_value pos (eval t1)) (int_of_value pos (eval t2))))
       | Ne -> (match t1 with 
           | BoolConst (b1, b2) -> BoolVal ((<>) (bool_of_value pos (eval t1)) (bool_of_value pos (eval t2)))
           | _ -> BoolVal ((<>) (int_of_value pos (eval t1)) (int_of_value pos (eval t2))))
       | Lt -> BoolVal ((<) (int_of_value pos (eval t1)) (int_of_value pos (eval t2)))   (* < *)
       | Gt -> BoolVal ((>) (int_of_value pos (eval t1)) (int_of_value pos (eval t2)))  (* > *)
       | Le -> BoolVal ((<=) (int_of_value pos (eval t1)) (int_of_value pos (eval t2)))  (* <= *)
       | Ge -> BoolVal ((>=) (int_of_value pos (eval t1)) (int_of_value pos (eval t2)))  (* >= *)
       | And -> BoolVal ((&&) (bool_of_value pos (eval t1)) (bool_of_value pos (eval t2)))  (* && *)
       | Or  -> BoolVal ((||) (bool_of_value pos (eval t1)) (bool_of_value pos (eval t2)))  (* || *)
      )
    | Ite (t1, t2, t3, pos) ->
      (* TODO: implement evaluation of conditional expressions. *)
      (match bool_of_value pos (eval t1) with
       | true -> eval t2
       | false -> eval t3
      )
    | Lambda (x, t, _) ->
      Closure (x, t, [])
    | Var (x, pos) ->
      (* This case should never be reachable assuming that the input term is closed 
       * and substitution is implemented correctly. *)
      fail pos "unexpected free variable"
  in
  eval t

(** beta-reduction step using applicative-order (call-by-value) semantics *)
let rec tranHelper t1 t2 =
  match t1 with 
  | Lambda (alpha, t, pos) -> subst t alpha t2
  | App (tt1, tt2, pos) -> tranHelper (tranHelper tt1 tt2) t2
  | _ -> t1

let rec beta_call_by_value (t1: term) (t2: term) (pos: pos) : value =
  (* TODO: implement beta reduction for the function application term `t1 t2`.
   * Make sure that your evaluation order yields call-by-value semantics. 
   * Hint: To evaluate t1 and/or t2, you can call 'eval' recursively using 'beta_call_by_value' as argument. *)
  match eval beta_call_by_value t1 with
  | Closure (x, t, []) -> eval beta_call_by_value (subst t x (term_of_value dummy_pos (eval beta_call_by_value t2)))
  | _ -> IntVal 1

(** Evaluate term t to a value using applicative-order (call-by-value) semantics *)
let eval_by_value (t: term) : value = eval beta_call_by_value t

(** beta-reduction step using normal-order (call-by-name) semantics *)
let rec beta_call_by_name  (t1: term) (t2: term) (pos: pos) : value =
  (* TODO: implement beta reduction for the function application term `t1 t2`.
   * Make sure that your evaluation order yields call-by-name semantics.
   * Hint: To evaluate t1 and/or t2, you can call 'eval' recursively using 'beta_call_by_name' as argument. *)
  match eval beta_call_by_name t1 with
  | Closure (x, t, []) -> eval beta_call_by_name (subst t x t2)
  | _ -> failwith "Type Error"


(** Evaluate term t to a value using normal-order (call-by-name) semantics *)
let eval_by_name (t: term) : value = eval beta_call_by_name t

(** {1} Bonus part *)

(** Evaluate term t using value environments instead of substitutions *)
let eval_with_envs (t: term) : value =
  let rec eval (env: env) (t: term) = 
    failwith "Not yet implemented" (* TODO: replace this with your implementation *)
  in
  eval [] t
