open Base
module O = Stdio.Out_channel
module I = Stdio.In_channel
module S = Syntax

(* 𝕄 = 𝕏 -> 𝕍 *)
type mem = S.const array

(* print_mem: mem -> unit *)
let print_mem m =
  Array.iteri m
    ~f:(fun i c ->
         if i > 0 then O.output_string O.stdout ", ";
         O.output_value O.stdout c);
  O.newline O.stdout

(* read: var -> mem -> const *)
let read x m = m.(x)

(* write: var -> const -> mem -> mem *)
let write x n m =
  let nm = Array.copy m in
  nm.(x) <- n;
  nm

type state = S.label * mem


(*
  〚E〛: 𝕄 -> 𝕍
  〚n〛(m) = n
  〚x〛(m) = m(x)
  〚E₀ ⊙ E₁〛(m) = f⊙(〚E₀〛(m),〚E₁〛(m))
*)

(* binop: bop -> const -> const -> const *)
let binop o v0 v1 =
  match o with
  | S.Badd -> v0 + v1
  | Bsub -> v0 - v1
  | Bmul -> v0 * v1

(* sem_expr : expr -> mem -> const*)
let rec sem_expr e m =
  match e with
  | S.Ecst n -> n
  | Evar x -> read x m
  | Ebop (o, e0, e1) ->
      binop o (sem_expr e0 m) (sem_expr e1 m)

(*
  〚B〛: 𝕄 -> 𝔹
  〚x ◶ n〛(m) = f◶(m(x),n)
*)

(* relop: rel -> const -> const -> bool *)
let relop c v0 v1 =
  match c with
  | S.Cinfeq -> v0 <= v1
  | Csup -> v0 > v1

(* sem_cond: (rel * var * const) -> mem -> bool *)
let sem_cond (c, x, n) m =
  relop c (read x m) n

(*
  〚C〛P: 𝒫(𝕄) -> 𝒫(𝕄)
  〚skip〛P(M) = M
  〚C₀;C₁〛P(M) = 〚C₁〛P(〚C₀〛P(M))
  〚x := E〛P(M) = { m[x ↦〚E〛(m)] | m ∈ M }
  〚input(x)〛P(M) = { m[x ↦ n] | m ∈ M, n ∈ 𝕍 }
  〚if(B){C₀}else{C₁}〛P(M) = 〚C₀〛P(ℱB(M)) ∪ 〚C₁〛P(ℱ¬B(M))
  〚while(B){C}〛P(M) = ℱ¬B(⋃_{i≥0} (〚C〛P ∘ ℱB)ⁱ(M))
*)

(* val sem_com : com -> mem -> mem *)
let rec sem_com (l, c) m =
  match c with
  | S.Cskip -> m
  | Cseq (c0, c1) -> sem_com c1 (sem_com c0 m)
  | Cassign (x, e) -> write x (sem_expr e m) m
  | Cinput x -> write x (Caml.read_int ()) m
  | Cif (b, c0, c1) ->
      if sem_cond b m then sem_com c0 m
      else sem_com c1 m
  | Cwhile (b, c) ->
      if sem_cond b m then
        sem_com (l, Cwhile (b, c)) (sem_com c m)
      else m

(* step: prog -> state -> state *)
let step p (l, m) =
  match S.find p l with
  | S.Cskip ->
      (S.next p l, m)
  | Cseq (_c0, _c1) ->
      (S.next p l, m)
  | Cassign (x, e) ->
      (S.next p l, write x (sem_expr e m) m)
  | Cinput x ->
      (S.next p l, write x (Caml.read_int ()) m)
  | Cif (b, c0, c1) ->
      if sem_cond b m then (fst c0, m)
      else                 (fst c1, m)
  | Cwhile (b, c) ->
      if sem_cond b m then (fst c, m)
      else                 (S.next p l, m)

(* iter: com -> mem -> mem *)
let iter c m =
  let p = S.com2prog c in
  let rec loop l m =
    let (l', m') = step p (l, m) in
    if l' = S.end_label then m'
    else loop l' m' in
  loop (S.first p) m
