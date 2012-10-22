(* expressions *)

exception Runtime ;;

(*type var = Var of string
and value = Con of int | Fun of var * expr
and expr = var = value = App of expr * expr
                         | Oper of string * (expr list)
                         | Set of var * expr
                         | Letrec of ((var * expr) list) * expr*)

(* Expressions definition and functions *)

type expr = Var of string
          | Fun of expr * expr
          | Con of int
          | App of expr * expr
          | Oper of string * (expr list)
          | Set of expr * expr
          | Letrec of ((expr * expr) list) * expr

(* expr_equal : expr expr -> bool *)
let rec expr_equal m n = match m, n with
| App (m, n), App (o, p) -> (expr_equal m o) && (expr_equal n p)
| Con i, Con j -> i = j
| Var s, Var t -> s = t
| Fun (x, m), Fun (y, n) -> (expr_equal x y) && (expr_equal m n)
| Oper (o1, l1), Oper (o2, l2) -> (o1 = o2) && (expr_list_equal l1 l2 true)
| Set (x, m), Set (y, n) -> (expr_equal x y) && (expr_equal m n)
| Letrec (l1, m), Letrec (l2, n) -> (letrec_clause_list_equal l1 l2 true) && (expr_equal m n)
| _ -> false
and expr_list_equal l1 l2 a = match l1, l2 with
| [], [] -> a
| h1::t1, h2::t2 -> expr_list_equal t1 t2 (a && (expr_equal h1 h2))
| _ -> false
and letrec_clause_list_equal l1 l2 a = match l1, l2 with
| [], [] -> a
| (x, m)::t1, (y, n)::t2 -> letrec_clause_list_equal t1 t2 (a && (expr_equal x y) && (expr_equal m n))
| _ -> false ;;

(* string_of_expr expr -> string *)
let rec string_of_expr x = match x with
|    Con i      -> string_of_int i
|    Var s      -> s
|    Fun (x, m) -> "(λ " ^ (string_of_expr x) ^ "." ^ (string_of_expr m) ^ ")"
|    App (m, n) -> "(" ^ (string_of_expr m) ^ " " ^ (string_of_expr n) ^ ")"
|   Oper (o, l) -> "(" ^ o ^ " " ^ (List.fold_right (fun n a -> a ^ (string_of_expr n)) l "") ^ ")"
|    Set (x, m) -> "(set " ^ (string_of_expr x) ^ " " ^ (string_of_expr m) ^ ")"
| Letrec (l, m) -> "(letrec " ^ (string_of_lrclause l "") ^ " " ^ (string_of_expr m) ^ ")"
and string_of_lrclause l a = match l with
| [] -> "(" ^ a ^ ")"
| (x, m)::cdr -> string_of_lrclause cdr (a ^ "(" ^ (string_of_expr x) ^ " " ^ (string_of_expr m) ^ ")") ;;

(* flat_map : ('a -> b' list) ('a list) ('b list) *)
let rec flat_map f l acc = match l with
| [] -> acc
| car::cdr -> flat_map f cdr (acc @ (f car)) ;;

(* av : expr -> Var list *)
let rec av m = match m with
|    Con i      -> []
|    Var s      -> []
|    Fun (v, n) -> List.filter (fun x -> not (expr_equal v x)) (av n)
|    App (n, o) -> (av n) @ (av o)
|   Oper (o, l) -> flat_map av l []
|    Set (x, n) -> x::(av n)
| Letrec (l, n) ->
  let xs = List.map (fun p -> match p with | (x, _) -> x) l in
  let test = (fun x -> List.fold_right (fun y a -> a || (expr_equal x y)) xs false) in
  List.filter test ((flat_map (fun p -> match p with | (x, o) -> av o) l []) @ (av n)) ;;

(* fv : expr -> Var list *)
let rec fv m = match m with
|    Con i      -> []
|    Var s      -> []
|    Fun (v, n) -> List.filter (fun x -> not (expr_equal v x)) (fv n)
|    App (n, o) -> (fv n) @ (fv o)
|   Oper (o, l) -> flat_map fv l []
|    Set (x, n) -> x::(fv n)
| Letrec (l, n) ->
  let xs = List.map (fun p -> match p with | (x, _) -> x) l in
  let test = (fun x -> List.fold_right (fun y a -> a || (expr_equal x y)) xs false) in
  List.filter test ((flat_map (fun p -> match p with | (x, o) -> fv o) l []) @ (fv n)) ;;

type loc = Loc of int

(* loc_equal : loc loc -> bool *)
let loc_equal l1 l2 = match l1, l2 with | Loc a, Loc b -> a = b ;;

type env = MtEnv
         | ConsEnv of expr * loc * env

(* apply : env Var -> loc *)
let rec apply e x = match e with
| MtEnv -> raise Runtime
| ConsEnv (y, l, e1) when expr_equal x y -> l
| ConsEnv (_, _, e1) -> apply e1 x ;;

(* bind : env Var loc -> env *)
let bind e x l = ConsEnv (x, l, e) ;;

(* domain : env -> Var list *)
let rec domain e = match e with
| MtEnv -> []
| ConsEnv (x, _, e1) -> x::(domain e1) ;;

(* range : env -> loc list *)
let rec range e = match e with
| MtEnv -> []
| ConsEnv (_, l, e1) -> l::(range e1) ;;

(* ll_env : env -> loc list *)
let ll_env e = match e with
| MtEnv -> []
| (_ : env) -> range e ;;

type closure = Closure of expr * env

(* ll_closure : closure -> loc list *)
let ll_closure c = match c with | Closure (m, e) -> ll_env e ;;

type store = MtStore
           | ConsStore of loc * (closure option) * store

(* apply : store loc -> closure *)
let rec apply s l = match s with
| ConsStore (l1, Some c, s1) when loc_equal l l1 -> c
| ConsStore (l1, _, s1) -> apply s1 l
| _ -> raise Runtime ;;

(* alloc : store loc -> store *)
let alloc s l = ConsStore (l, None, s) ;;

(* bind : store loc closure -> store *)
let bind s l c = ConsStore (l, Some c, s) ;;

(* rebind : store loc closure -> store *)
let rec rebind s l c = match s with
| MtStore -> MtStore
| ConsStore (l1, _, s1) when l = l1 -> ConsStore (l1, Some c, s1)
| ConsStore (l1, o, s1) -> ConsStore (l1, o, rebind s1 l c) ;;

(* next : loc -> loc *)
let next l = match l with | Loc n -> Loc (n + 1) ;;

(* next : store -> loc *)
let next s = match s with
| MtStore -> Loc 0
| ConsStore (l1, _, _) -> next l1 ;;

(* domain : store -> loc list *)
let rec domain s = match s with
| MtStore -> []
| ConsStore (l, _, s1) -> l::(domain s1) ;;

(* range : store -> closure list *)
let rec range s = match s with
| MtStore -> []
| ConsStore (_, Some c, e1) -> c::(range e1)
| ConsStore (_, None, e1) -> range e1 ;;

type kont = MtKont
          | Fn of closure * kont
          | Ar of closure * kont
          | Op of string * (closure list) * (closure list) * kont
          | St of loc * kont
          | Lr of (expr list) * ((expr * expr) list) * (expr list) * env * expr * kont

(* ll_kont : kont -> loc list *)
let rec ll_kont k = match k with
| MtKont -> []
| Fn (c, k) -> (ll_closure c) @ (ll_kont k)
| Ar (c, k) -> (ll_closure c) @ (ll_kont k)
| Op (_, vs, cs, k) -> (flat_map ll_closure vs []) @ (flat_map ll_closure cs []) @ (ll_kont k)
| St (l, k) -> l::(ll_kont k)
| Lr (_, _, _, e, _, k) -> (ll_env e) @ (ll_kont k) ;;

let rec eval_cesk ce s k = match ce, s, k with
(* cesk1 *)
| Closure (App (m,  n), e), s, k ->
  eval_cesk (Closure (m, e)) s (Ar (Closure (n, e), k))
(* cesk2 *)
| Closure (Oper (o, m::ms), e), s, k ->
  eval_cesk (Closure (m, e)) s (Op (o, [], List.map (fun x -> Closure (x, e)) ms, k))
(* cesk3 *)
(*| Closure ((v : value), e), s, Fn (Closure (Fun (x, m), e1), k1) ->
  let l = next s in eval_cesk (Closure (m, bind e1 x l)) (bind s l (Closure (v, e))) k1
*)
;;

let eval m = eval_cesk (Closure (m, MtEnv)) MtStore MtKont ;;
