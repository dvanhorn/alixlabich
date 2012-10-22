(* expressions *)

type expr = Con of int
          | Var of string
          | Fun of expr * expr
          | App of expr * expr
          | Oper of string * (expr list)
          | Set of expr * expr
          | Letrec of ((expr * expr) list) * expr

let rec expr_equal m n = match m, n with
| Con i, Con j -> i = j
| Var s, Var t -> s = t
| Fun (x, m), Fun (y, n) -> (expr_equal x y) && (expr_equal m n)
| App (m, n), App (o, p) -> (expr_equal m o) && (expr_equal n p)
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

let rec flat_map f l acc = match l with
| [] -> acc
| car::cdr -> flat_map f cdr (acc @ (f car)) ;;


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

print_endline (string_of_expr (Letrec ([(Var "x", Fun (Var "x", Var "x"))], App (Var "x", Con 1))))

