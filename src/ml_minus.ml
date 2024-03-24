(* Do not open any module *)



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
      | _ -> raise UndefinedSemantics)
  | SUB (e1,e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1,v2 with
      | Int n1, Int n2 -> Int (n1 - n2)
      | _ -> raise UndefinedSemantics)
  | MUL (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
        | Int n1, Int n2 -> Int (n1 * n2)
      | _ -> raise UndefinedSemantics)
  | DIV (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | Int n1, Int n2 -> if n2 != 0 then Int (n1 / n2) else raise UndefinedSemantics
      | _ -> raise UndefinedSemantics)
  | EQUAL (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
      | Int n1, Int n2 -> Bool (n1 = n2)
      | Bool b1, Bool b2 -> Bool (b1 = b2)
      | List s1, List s2 -> Bool (s1 = s2) (* 반드시 확인해볼 것*)
      | _ -> raise UndefinedSemantics)
  | NOT (e1) ->
    let v1 = eval e1 env in
      (match v1 with
      | Bool b1 -> Bool (not b1)
      | _ -> raise UndefinedSemantics)
  | NIL -> List []
  | CONS (e1, e2) ->
    let v1 = eval e1 env in 
    let v2 = eval e2 env in
      (match v1, v2 with
      | value , List s1 -> List (v1::s1) (* 확인 필요*)
      | _ -> raise UndefinedSemantics)
  | APPEND (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
        | List s1, List s2 -> List (s1 @ s2)
      | _ -> raise UndefinedSemantics)
  | HEAD (e1) ->
    let v1 = eval e1 env in
      (match v1 with
      | List s1 -> (match s1 with
                    | [] -> raise UndefinedSemantics
                    | hd::tl -> hd)
      | _ -> raise UndefinedSemantics)
  | TAIL (e1) ->
    let v1 = eval e1 env in
      (match v1 with
      | List s1 -> (match s1 with
                    | [] -> raise UndefinedSemantics
                    | hd::tl -> List tl)
      | _ -> raise UndefinedSemantics)
  | ISNIL (e1) ->
    let v1 = eval e1 env in
      (match v1 with
      | List s1 -> (match s1 with
                    | [] -> Bool true
                    | hd::tl -> Bool false)
      | _ -> raise UndefinedSemantics)
  | LESS (e1, e2) ->
    let v1 = eval e1 env in
    let v2 = eval e2 env in
      (match v1, v2 with
        | Int n1, Int n2 -> Bool (n1 < n2)
      | _ -> raise UndefinedSemantics)
  | IF (e1,e2,e3) ->
    (match eval e1 env with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise UndefinedSemantics)
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
      | _ -> raise UndefinedSemantics
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
  | _ -> raise UndefinedSemantics

let runml : program -> value
=fun pgm -> eval pgm empty_env


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
and tenv = (var * typ) list

type typ_eqn = (typ * typ) list

exception TypeError
(* You can invoke "fresh_tyvar()" to generate a fresh type variable *)
let tyvar_num = ref 0
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))


(* substitution *)
module Subst = struct
  type t = (tyvar * typ) list
  let empty = []
  let find x subst = List.assoc x subst
  let rec apply : typ -> t -> typ
  =fun typ subst ->
    match typ with
    | TyUnit -> TyUnit
    | TyList t -> TyList (apply t subst)
    | TyInt -> TyInt
    | TyBool -> TyBool
    | TyFun (t1,t2) ->
      TyFun (apply t1 subst, apply t2 subst)
    | TyVar x ->
      try find x subst
      with _ -> typ
  let extend tv ty subst =
    (tv,ty) ::
    (List.map (fun (x,t) ->
      (x, apply t [(tv,ty)])) subst) (* Propagation *)
end

(* 강의 자료에서의 V() 함수 *)
let rec gen_equations : tenv -> exp -> typ -> typ_eqn
=fun tenv exp ty -> (* TODO *)
  match exp with
  | UNIT -> [ty, TyUnit]
  | CONST n -> [ty, TyInt]
  | TRUE -> [ty, TyBool]
  | FALSE -> [ty, TyBool]
  | VAR x -> [ty, lookup_env x tenv]
  | ADD (e1, e2) -> (ty, TyInt) :: (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | SUB (e1, e2) -> (ty, TyInt) :: (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | MUL (e1, e2) -> (ty, TyInt) :: (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | DIV (e1, e2) -> (ty, TyInt) :: (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | EQUAL (e1, e2) -> let te = fresh_tyvar() in (ty, TyBool) :: (gen_equations tenv e1 te) @ (gen_equations tenv e2 te)
  | LESS (e1, e2) -> (ty, TyBool) :: (gen_equations tenv e1 TyInt) @ (gen_equations tenv e2 TyInt)
  | NOT e1 -> (ty, TyBool) :: (gen_equations tenv e1 TyBool)
  | NIL -> [ty, TyList (fresh_tyvar())]
  | CONS (e1, e2) -> let te = fresh_tyvar() in (ty, (TyList te)) :: (gen_equations tenv e1 te) @ (gen_equations tenv e2 (TyList te))
  | APPEND (e1, e2) -> let te = fresh_tyvar() in (ty, TyList te) :: (gen_equations tenv e1 (TyList te)) @ (gen_equations tenv e2 (TyList te))
  | HEAD e1 -> let te = fresh_tyvar() in (ty, te) :: (gen_equations tenv e1 (TyList te)) 
  | TAIL e1 -> let te = fresh_tyvar() in (ty, TyList te) :: (gen_equations tenv e1 (TyList te))
  | ISNIL e1 -> let te = fresh_tyvar() in (ty, TyBool) :: (gen_equations tenv e1 (TyList te))
  | IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool) @ (gen_equations tenv e2 ty) @ (gen_equations tenv e3 ty)
  | LET (x, e1, e2) -> let t1 = fresh_tyvar() in (gen_equations tenv e1 t1) @ (gen_equations (extend_env (x, t1) tenv) e2 ty)
  | LETREC (f, x, e1, e2) -> 
    let tx = fresh_tyvar() in
    let t1 = fresh_tyvar() in
    let t2 = fresh_tyvar() in
    (ty, t2) :: (gen_equations (extend_env (f, TyFun (tx, t1)) (extend_env (x, tx) tenv)) e1 t1) @ (gen_equations (extend_env (f, TyFun (tx, t1)) tenv) e2 t2)
  | LETMREC ((f, x, e1), (g, y, e2), e3) ->
    let tx = fresh_tyvar() in
    let tg = fresh_tyvar() in
    let t1 = fresh_tyvar() in
    let t2 = fresh_tyvar() in
    let t3 = fresh_tyvar() in
    (ty, t3) 
    :: (gen_equations (extend_env (f, TyFun (tx, t1)) (extend_env (g, TyFun (tg, t2)) (extend_env (x, tx) tenv))) e1 t1) 
    @ (gen_equations (extend_env (f, TyFun (tx, t1)) (extend_env (g, TyFun (tg, t2)) (extend_env (y, tg) tenv))) e2 t2) 
    @ (gen_equations (extend_env (f, TyFun (tx, t1)) (extend_env (g, TyFun (tg, t2)) tenv)) e3 t3)       
  | CALL (e1, e2) -> 
    let t1 = fresh_tyvar() in 
    let t2 = fresh_tyvar() in
    (ty, t2) 
    :: (gen_equations tenv e1 (TyFun (t1, t2)))
    @ (gen_equations tenv e2 t1)
  | PROC (x, e1) -> 
    let t1 = fresh_tyvar() in 
    let t2 = fresh_tyvar() in
    (ty, TyFun (t1, t2)) :: (gen_equations (extend_env (x, t1) tenv) e1 t2)
  | PRINT e1 -> 
    let t1 = fresh_tyvar() in
    (ty, TyUnit) :: (gen_equations tenv e1 t1)
  | SEQ (e1, e2) ->
    let t1 = fresh_tyvar() in
    let t2 = fresh_tyvar() in
    (ty, t2) :: 
    (gen_equations tenv e1 t1) @
    (gen_equations tenv e2 t2)


let rec occur : tyvar  -> typ -> bool
= fun alpha t ->
  match t with
  | TyUnit | TyInt | TyBool -> false
  | TyVar alpha2 -> if alpha = alpha2 then raise TypeError else false
  | TyList t -> occur alpha t
  | TyFun (t1, t2) -> (occur alpha t1) || (occur alpha t2)

(* 가정 : Apply를 거쳐서 들어온다. *)
(* 
1. Occurence Check
2. List *)
let rec unify : typ -> typ -> Subst.t -> Subst.t
= fun t1 t2 subst ->
  match t1, t2 with
  | TyUnit, TyUnit -> subst
  | TyInt, TyInt -> subst
  | TyBool, TyBool -> subst
  | TyList t1, TyList t2 -> unify t1 t2 subst
  | TyVar alpha, t | t, TyVar alpha ->
    (match t with
    | TyVar alpha2 ->
      if alpha = alpha2 then subst
      else Subst.extend alpha t subst
    | _ ->     (* Occurence Check *)
    if occur alpha t then raise TypeError
    else Subst.extend alpha t subst)
  | TyFun (t1, t2), TyFun (t3, t4) ->
    let subst' = unify t1 t3 subst in
    unify (Subst.apply t2 subst') (Subst.apply t4 subst') subst'
  | _ -> raise TypeError 

(* solve == unifyall *)
let rec solve : typ_eqn -> Subst.t -> Subst.t
=fun eqn sub -> 
    match eqn with
    | [] -> sub
    | hd::tl ->
      match hd with
      | (x, t) -> let sub_ = ( unify (Subst.apply x sub) (Subst.apply t sub) sub ) in solve tl sub_

let typecheck : program -> typ
=fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations empty_env exp new_tv in
  let subst = solve eqns Subst.empty in 
  let ty = Subst.apply new_tv subst in ty;;


(* runml proc1;;
typecheck proc1;;
runml proc2;;
typecheck proc2;;
runml let1;;
typecheck let1;;
runml evenodd;;
typecheck evenodd;;
runml double;;
typecheck double;;
runml fact;;
typecheck fact;;
runml range;;
typecheck range;;
runml reverse;;
typecheck reverse;;
runml reverse2;;
typecheck reverse2;;
runml zfact;;
typecheck zfact;;
runml zrange;;
typecheck zrange;;
runml poly;;
typecheck poly;;
runml lst;;
typecheck lst;; *)

let fact = 
LETREC ("factorial", "x", 
          IF (EQUAL (VAR "x", CONST 0), CONST 1, 
              MUL (CALL (VAR "factorial", SUB (VAR "x", CONST 1)), VAR "x")), 
  LETREC ("loop", "n", 
    IF (EQUAL (VAR "n", CONST 0), UNIT, 
        SEQ (PRINT (CALL (VAR "factorial", VAR "n")), 
             CALL (VAR "loop", SUB(VAR "n", CONST 1)))), 
      CALL (VAR "loop", CONST 10)));;

runml fact;;
typecheck fact;;