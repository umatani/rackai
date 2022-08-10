open Base

(*
  n ∈ 𝕍
  x ∈ 𝕏
  ⊙ ::= + | - | * | ...
  ◶ ::= < | ≤ | == | ...
  E ::= scalar expressions
      | n
      | x
      | E ⊙ E
  B ::= Boolean expressions
      | x ◶ n
  C ::= commands
      | skip
      | C; C
      | x := E
      | input(x)
      | if(B) { C } else { C }
      | while(B) { C }
*)

type label = int
type const = int
type var = int
type bop = Badd | Bsub | Bmul
type rel = Cinfeq | Csup

type expr =
  | Ecst of const
  | Evar of var
  | Ebop of bop * expr * expr

type cond = rel * var * const

type command =
  | Cskip
  | Cseq of com * com
  | Cassign of var * expr
  | Cinput of var
  | Cif of cond * com * com
  | Cwhile of cond * com
and com = label * command

let end_label = -1

(* cneg: cond -> cond *)
let cneg (r, x, n) =
  match r with
  | Cinfeq -> (Csup, x, n)
  | Csup -> (Cinfeq, x, n)

(* type prog = (label * command * label) list *)
type prog = label * (label, command * label) Hashtbl.t

(* find : prog -> label -> command *)
let find p l = fst (Hashtbl.find_exn (snd p) l)

(* next : prog -> label -> label *)
let next p l = snd (Hashtbl.find_exn (snd p) l)

let com2prog (l, c) =
  let rec aux p (l, c) endl =
    match c with
    | Cskip -> Hashtbl.add_exn p ~key:l ~data:(c, endl)
    | Cseq ((l0, c0), (l1, c1)) ->
        aux p (l0, c0) l1;
        aux p (l1, c1) endl;
        Hashtbl.add_exn p ~key:l ~data:(c, l0)
    | Cassign (_v, _e) -> Hashtbl.add_exn p ~key:l ~data:(c, endl)
    | Cinput _v -> Hashtbl.add_exn p ~key:l ~data:(c, endl)
    | Cif (_b, (l0, c0), (l1, c1)) ->
        aux p (l0, c0) endl;
        aux p (l1, c1) endl;
    | Cwhile (_b, (l0, c0)) ->
        aux p (l0, c0) l;
        Hashtbl.add_exn p ~key:l ~data:(c, endl) in
  let h = (Hashtbl.create (module Int)) in
  aux h (l, c) (-1);
  (l, h)

(* first: prog -> label *)
let first p = fst p
