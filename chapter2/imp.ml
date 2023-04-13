(* Excersize 2.1 *)

type loc = string

type aexp =
  | Const of int  (* n *)
  | Var of loc  (* X *)
  | Add of aexp * aexp  (* a0 + a1 *)
  | Sub of aexp * aexp  (* a0 - a1 *)
  | Mul of aexp * aexp  (* a0 * a1 *)

type bexp =
  | True  (* true *)
  | False  (* false *)
  | Eq of aexp * aexp  (* a0 = a1 *)
  | Le of aexp * aexp  (* a0 <= a1 *)
  | Not of bexp  (* not b *)
  | And of bexp * bexp  (* b0 and b1 *)
  | Or of bexp * bexp  (* b0 or b1 *)

type com =
  | Skip  (* skip *)
  | Assign of loc * aexp  (* X := a *)
  | Seq of com * com  (* c0; c1 *)
  | If of bexp * com * com  (* if b then c0 else c1 *)
  | While of bexp * com  (* while b do c *)


let rec aexp_equal a1 a2 =
  match (a1, a2) with
  | (Const i1, Const i2) -> i1 = i2
  | (Var v1, Var v2) -> v1 = v2
  | (Add (a11, a12), Add (a21, a22)) -> (aexp_equal a11 a21) && (aexp_equal a12 a22)
  | (Sub (a11, a12), Sub (a21, a22)) -> (aexp_equal a11 a21) && (aexp_equal a12 a22)
  | (Mul (a11, a12), Mul (a21, a22)) -> (aexp_equal a11 a21) && (aexp_equal a12 a22)
  | _ -> false

let rec bexp_equal b1 b2 =
  match (b1, b2) with
  | (True, True) -> true
  | (False, False) -> true
  | (Eq (a11, a12), Eq (a21, a22)) -> (aexp_equal a11 a21) && (aexp_equal a12 a22)
  | (Le (a11, a12), Le (a21, a22)) -> (aexp_equal a11 a21) && (aexp_equal a12 a22)
  | (Not b1, Not b2) -> bexp_equal b1 b2
  | (And (b11, b12), And (b21, b22)) -> (bexp_equal b11 b21) && (bexp_equal b12 b22)
  | (Or (b11, b12), Or (b21, b22)) -> (bexp_equal b11 b21) && (bexp_equal b12 b22)
  | _ -> false

let rec com_equal c1 c2 =
  match (c1, c2) with
  | (Skip, Skip) -> true
  | (Assign (v1, a1), Assign (v2, a2)) -> (v1 = v2) && (aexp_equal a1 a2)
  | (Seq (c11, c12), Seq (c21, c22)) -> (com_equal c11 c21) && (com_equal c12 c22)
  | (If (b1, c11, c12), If (b2, c21, c22)) -> (bexp_equal b1 b2) && (com_equal c11 c21) && (com_equal c12 c22)
  | (While (b1, c1), While (b2, c2)) -> (bexp_equal b1 b2) && (com_equal c1 c2)
  | _ -> false


let test1 () =
  print_endline (Bool.to_string (com_equal (Assign ("X", Const 1)) (Assign ("X", Const 1))));
  print_endline (Bool.to_string (com_equal (Assign ("X", Const 1)) (Assign ("X", Const 2))));
;;


test1();

(* Excersize 2.2 *)

type state = (loc, int) Hashtbl.t

let rec eval_aexp (a: aexp) (s: state) : int =
  match a with
  | Const n -> n
  | Var x -> Hashtbl.find s x
  | Add (a1, a2) -> eval_aexp a1 s + eval_aexp a2 s
  | Sub (a1, a2) -> eval_aexp a1 s - eval_aexp a2 s
  | Mul (a1, a2) -> eval_aexp a1 s * eval_aexp a2 s


let test2 () =
  let s = Hashtbl.create 0 in
  Hashtbl.add s "X" 1;
  print_endline (Int.to_string (eval_aexp (Const 1) s));
  print_endline (Int.to_string (eval_aexp (Add (Const 1, Var "X")) s));
;;


test2();
