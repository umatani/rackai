open Base
module S = Syntax

module Dom_sign = struct
  type t =
  | Abot
  | Atop
  | Apos
  | Aneg
  [@@deriving show]

  let val_bot = Abot (* val_bot: t *)
  let val_top = Atop (* val_top: t *)

  (* val_incl: t -> t -> bool *)
  let val_incl a0 a1 = Poly.(a0 = Abot || a1 = Atop || a0 = a1)

  (* val_cst: const -> t *)
  let val_cst n = if n < 0 then Aneg else Apos

  (* val_sat: rel -> int -> t -> t *)
  let val_sat o n a =
    if Poly.(a = Abot)
    then Abot
    else if Poly.(o = S.Cinfeq) && n < 0 then
      if Poly.(a = Apos) then Abot else Aneg
    else if Poly.(o = Csup) && n >= 0 then
      if Poly.(a = Aneg) then Abot else Apos
    else a

  (* val_join: t -> t -> t *)
  let val_join a0 a1 =
    match a0, a1 with
    | Abot, a | a, Abot -> a
    | Atop, _ | _, Atop | Apos, Aneg | Aneg, Apos -> Atop
    | Apos, Apos -> Apos
    | Aneg, Aneg -> Aneg

  (* val_binop: bop -> t -> t -> t *)
  let val_binop o a0 a1 =
    match o, a0, a1 with
    | _, Abot, _ | _, _, Abot -> Abot
    | S.Badd, Apos, Apos -> Apos
    | Badd, Aneg, Aneg -> Aneg
    | Badd, _, _ -> Atop
    | Bsub, Apos, Aneg -> Apos
    | Bsub, Aneg, Apos -> Aneg
    | Bsub, _, _ -> Atop
    | Bmul, Apos, Apos -> Apos
    | Bmul, Apos, Aneg -> Aneg
    | Bmul, Aneg, Apos -> Aneg
    | Bmul, Aneg, Aneg -> Apos
    | Bmul, _, _ -> Atop
end