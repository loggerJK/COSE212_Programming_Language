(* Do not open any module *)
(* TODO
1. Undefined Semantics
2. Type Checker   
*)


(***********************)
(*  Library functions  *)
(***********************)
let rec fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
= fun f lst accu ->
  match lst with
  | [] -> accu
  | hd::tl -> f hd (fold_right f tl accu)

let rec fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
= fun f accu lst ->
  match lst with
  | [] -> accu
  | hd::tl -> fold_left f (f accu hd) tl

let rec map : ('a -> 'b) -> 'a list -> 'b list
= fun f lst ->
  match lst with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

(***********************)
(******  Syntax  *******)
(***********************)

type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

(***********************)
(**  Example programs **)
(***********************)

(*
  let f = proc (x) (x - 11)
  in (f (f 77))

  let f = fun x -> (x - 11)
  in (f (f 77))
*)
let proc1 = 
  LET ("f", PROC ("x", SUB (VAR "x", CONST 11)),
    CALL (VAR "f", CALL (VAR "f", CONST 77)))

(*
  ((proc (f) (f (f 77))) (proc (x) (x-11)))
*)
let proc2 = 
  CALL (PROC ("f", CALL (VAR "f", CALL (VAR "f", CONST 77))), 
        PROC ("x", SUB (VAR "x", CONST 11)))

(*
  let x = 1
  in let f = proc (y) (x + y)
     in let x = 2
        in let g = proc (y) (x + y)
        in  (f 1) + (g 1)
*)
let let1 = 
  LET ("x", CONST 1, 
    LET ("f", PROC ("y", ADD (VAR "x", VAR "y")),
      LET ("x", CONST 2, 
         LET ("g", PROC ("y", ADD (VAR "x", VAR "y")), 
            (ADD (CALL (VAR "f", CONST 1), 
                  CALL (VAR "g", CONST 1)))))))

(*
  letrec even(x) = if (x = 0) then true else odd(x-1)
         odd(x) = if (x = 0) then false else even(x-1)
  in (even 13)
*)
let evenodd = 
  LETMREC (("even", "x", IF (EQUAL (VAR "x", CONST 0), TRUE, CALL (VAR "odd",  SUB (VAR "x", CONST 1)))),
           ("odd" , "x", IF (EQUAL (VAR "x", CONST 0), FALSE, CALL (VAR "even", SUB (VAR "x", CONST 1)))),
  CALL (VAR "odd", CONST 13))


(*
  letrec double(x) = if (x = 0) then 0 else (double (x-1) + 2
  in (double 6)
*)
let double = 
  LETREC ("double", "x", IF (EQUAL (VAR "x", CONST 0), 
                            CONST 0, 
                            ADD (CALL (VAR "double", SUB (VAR "x", CONST 1)) , 
                                 CONST 2)), 
    CALL (VAR "double", CONST 6))

(*
letrec factorial(x) = 
         if (x = 0) then 1 
         else factorial(x-1) * x
in letrec loop n = 
     if (n = 0) then ()
     else (print (factorial n); loop (n-1))
   in (loop 10)
*)
let fact = 
LETREC ("factorial", "x", 
          IF (EQUAL (VAR "x", CONST 0), CONST 1, 
              MUL (CALL (VAR "factorial", SUB (VAR "x", CONST 1)), VAR "x")), 
  LETREC ("loop", "n", 
    IF (EQUAL (VAR "n", CONST 0), UNIT, 
        SEQ (PRINT (CALL (VAR "factorial", VAR "n")), 
             CALL (VAR "loop", SUB(VAR "n", CONST 1)))), 
      CALL (VAR "loop", CONST 10)))
           
(*
in letrec range(n) = 
      if (n = 1) then (cons 1 nil)
      else n::(range (n-1))
in (range 10)
*)
let range = 
LETREC ("range", "n", 
            IF (EQUAL (VAR "n", CONST 1), CONS (CONST 1, NIL),
                CONS (VAR "n", CALL (VAR "range", SUB (VAR "n", CONST 1)))), 
     CALL (VAR "range", CONST 10))

(*
letrec reverse(l) = 
  if (isnil l) then []
  else (reverse (tl l)) @ (cons hd l)
in (reverse (cons (1, cons (2, cons (3, nil)))))
*)
let reverse = 
LETREC ("reverse", "l", 
          IF (ISNIL (VAR "l"), NIL, 
              APPEND (CALL (VAR "reverse", TAIL (VAR "l")), 
                      CONS (HEAD (VAR "l"), NIL))), 
     CALL (VAR "reverse", 
           CONS (CONST 1, CONS (CONST 2, CONS (CONST 3, NIL)))))

let reverse2 = 
LETREC ("reverse", "l", 
          IF (ISNIL (VAR "l"), NIL, 
              APPEND (CALL (VAR "reverse", TAIL (VAR "l")), 
                      CONS (HEAD (VAR "l"), NIL))), 
     CALL (VAR "reverse", 
           CONS (CONS (CONST 1, NIL), CONS (CONS (CONST 2, NIL), CONS (CONS (CONST 3, NIL), NIL)))))


let zfact = 
  LET ("fix", 
    PROC ("f", 
      CALL (PROC ("x", CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))), 
            PROC ("x", CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))))),
    LET ("f", CALL (VAR "fix", 
            PROC ("f", PROC ("x", 
          IF (EQUAL (VAR "x", CONST 0), CONST 1, 
              MUL (CALL (VAR "f", SUB (VAR "x", CONST 1)), VAR "x"))))), 
           CALL (VAR "f", CONST 10)))

let zrange = 
  LET ("fix", 
    PROC ("f", 
      CALL (PROC ("x", CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))), 
            PROC ("x", CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))))),


    LET ("f", CALL (VAR "fix", 
            PROC ("range", PROC ("n", 
               IF (EQUAL (VAR "n", CONST 1), CONS (CONST 1, NIL),
                 CONS (VAR "n", CALL (VAR "range", SUB (VAR "n", CONST 1))))))), 
           CALL (VAR "f", CONST 10)))

let poly = 
    LET ("f", PROC("x", VAR "x"), 
      IF(CALL (VAR "f", TRUE), CALL (VAR "f", CONST 1), CALL (VAR "f", CONST 2)))

let lst =
    CONS (CONST 1, CONS (CONST 2, CONS (TRUE, NIL)))


(***********************)
(*****  Problem 1  *****)
(***********************)

type value = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | List of value list
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | MRecProcedure of var * var * exp *
                     var * var * exp * env
and env = (var * value) list

exception UndefinedSemantics

let rec string_of_value v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | List lst -> "[" ^ fold_left (fun s x -> s ^ ", " ^ x) "" (map string_of_value lst) ^ "]"
  | _ -> "(functional value)"

let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec lookup_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env")) 
  | (y,v)::tl -> if x = y then v else lookup_env x tl

let rec eval : exp -> env -> value
=fun exp env ->
  match exp with
  | UNIT -> Unit
  | TRUE -> Bool true
  | FALSE -> Bool false
  | CONST n -> Int n
  | VAR x -> lookup_env x env
  | ADD (e1,e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1,v2 with
      | Int n1, Int n2 -> Int (n1 + n2)
      | _ -> raise (Failure "Type Error"))
  | SUB (e1,e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1,v2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> raise (Failure "Type Error"))
  | MUL (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
        | Int n1, Int n2 -> Int (n1 * n2)
      | _ -> raise (Failure "Type Error"))
  | DIV (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | Int n1, Int n2 -> if n2 != 0 then Int (n1 / n2) else raise (Failure "Division by 0")
      | _ -> raise (Failure "Type Error"))
  | EQUAL (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | Int n1, Int n2 -> Bool (n1 = n2)
      | Bool b1, Bool b2 -> Bool (b1 = b2)
      | List s1, List s2 -> Bool (s1 = s2) (* 반드시 확인해볼 것*)
      | _ -> raise (Failure "Type Error"))
  | NOT (e1) ->
    let v1 = eval e1 env in
      (match v1 with
      | Bool b1 -> Bool (not b1)
      | _ -> raise (Failure "Type Error"))
  | NIL -> List []
  | CONS (e1, e2) ->
    let v1 = eval e1 env in 
    let v2 = eval e2 env in
      (match v1, v2 with
      | value , List s1 -> List (v1::s1) (* 확인 필요*)
      | _ -> raise (Failure "Type Error"))
  | APPEND (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
        | List s1, List s2 -> List (s1 @ s2)
      | _ -> raise (Failure "Type Error"))
  | HEAD (e1) ->
    let v1 = eval e1 env in
      (match v1 with
      | List s1 -> (match s1 with
                    | [] -> raise (Failure "Empty List")
                    | hd::tl -> hd)
      | _ -> raise (Failure "Type Error"))
  | TAIL (e1) ->
    let v1 = eval e1 env in
      (match v1 with
      | List s1 -> (match s1 with
                    | [] -> raise (Failure "Empty List")
                    | hd::tl -> List tl)
      | _ -> raise (Failure "Type Error"))
  | ISNIL (e1) ->
    let v1 = eval e1 env in
      (match v1 with
      | List s1 -> (match s1 with
                    | [] -> Bool true
                    | hd::tl -> Bool false)
      | _ -> raise (Failure "Type Error"))
  | LESS (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
        | Int n1, Int n2 -> Bool (n1 < n2)
      | _ -> raise (Failure "Type Error"))
  | IF (e1,e2,e3) ->
    (match eval e1 env with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise (Failure "Type Error"))
  | LET (x,e1,e2) ->
    let v1 = eval e1 env in
      eval e2 (extend_env (x,v1) env)
  | LETREC (f,x,e1,e2) ->
    let v1 = RecProcedure (f,x,e1,env) in
      eval e2 (extend_env (f,v1) env)
  | PROC (x,e1) -> Procedure (x,e1,env)
  | CALL (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1 with
      | Procedure (x,e1,env1) -> eval e1 (extend_env (x,v2) env1)
      | RecProcedure (f,x,e1,env1) -> eval e1 (extend_env (f,v1) (extend_env (x,v2) env1))
      | MRecProcedure (f,x,e_f,g,y,e_g,env_f) -> eval e_f (extend_env (g, MRecProcedure(g,y,e_g,f,x,e_f,env_f)) (extend_env (f,v1) (extend_env (x,v2) env_f)))
      | _ -> raise (Failure "Type Error")
      )
  | LETMREC ((f,x,e1),(g,y,e2),e3) ->
    let v1 = MRecProcedure(f,x,e1,g,y,e2,env) in
    let v2 = MRecProcedure(g,y,e2,f,x,e1,env) in
      eval e3 (extend_env (f,v1) (extend_env (g,v2) env))
  | SEQ (e1,e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      v2
  | PRINT e -> (print_endline (string_of_value (eval e env)); Unit)
  | _ -> raise (Failure "Not implemented") (* TODO *)

let runml : program -> value
=fun pgm -> eval pgm empty_env

(***********************)
(*****  Run TC     *****)
(***********************)

(***********************)
(*****  Problem 2  *****)
(***********************)

type typ = 
    TyUnit 
  | TyInt 
  | TyBool 
  | TyFun of typ * typ 
  | TyList of typ
  | TyVar of tyvar
and tyvar = string

exception TypeError

(* You can invoke "fresh_tyvar()" to generate a fresh type variable *)
let tyvar_num = ref 0
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

let typecheck : program -> typ 
=fun exp -> raise TypeError;; 