open Base
module S = Syntax

type t = Abot | Acst of int | Atop

let val_bot = Abot
let val_top = Atop

let val_incl a0 a1 = Poly.(a0 = Abot || a1 = Atop || a0 = a1)

let val_cst n = Acst n

let val_sat o n a =
  match o, a with
  | _, Abot -> Abot
  | S.Cinfeq, Acst i -> if i > n then Abot else a
  | Csup, Acst i -> if i <= n then Abot else a
  | _, Atop -> Atop

let val_join a0 a1 =
  match a0, a1 with
  | Abot, a | a, Abot -> a
  | Atop, _ | _, Atop -> Atop
  | Acst _, Acst _ ->  if Poly.(a0 = a1) then a0 else Atop

let val_binop o a0 a1 =
  match o, a0, a1 with
  | _, Abot, _ | _, _, Abot -> Abot
  | S.Badd, Atop, _ | Badd, _, Atop -> Atop
  | Badd, Acst i0, Acst i1 -> Acst (i0 + i1)
  | Bsub, Atop, _ | Bsub, _, Atop -> Atop
  | Bsub, Acst i0, Acst i1 -> Acst (i0 - i1)
  | Bmul, Atop, _ | Bmul, _, Atop -> Atop
  | Bmul, Acst i0, Acst i1 -> Acst (i0 * i1)
