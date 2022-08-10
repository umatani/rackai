open Base

module C = Conc
module S = Syntax

module Make (D : Dom.M) = struct

  type nr_abs = D.t array

  (* nr_init: int -> nr_abs *)
  let nr_init len = Array.create ~len:len D.val_top

  (* nr_bot: nr_abs -> nr_abs *)
  let nr_bot aenv = Array.map ~f:(fun _ -> D.val_bot) aenv

  (* nr_is_bot: nr_abs -> bool *)
  let nr_is_bot aenv =
    Array.exists ~f:(fun a -> Poly.(a = D.val_bot)) aenv

  (* nr_is_le: nr_abs -> nr_abs -> bool *)
  let nr_is_le aenv0 aenv1 =
    let r = ref true in
    Array.iteri
      ~f:(fun x a0 -> r := !r && D.val_incl a0 (C.read x aenv1))
      aenv0;
    !r

  (* nr_join: nr_abs -> nr_abs -> nr_abs *)
  let nr_join aenv0 aenv1 =
    Array.mapi
      ~f:(fun x a0 -> D.val_join a0 (C.read x aenv1))
      aenv0


  (*
    〚E〛#: 𝔸 -> 𝔸v
    〚n〛#(M#) = ϕv(n)
    〚x〛#(M#) = M#(x)
    〚E₀ ⊙ E₁〛#(M#) = f#⊙(〚E₀〛#(M#),〚E₁〛#(M#))
  *)

  (* ai_expr: expr -> nr_abs -> val_abs *)
  let rec ai_expr e aenv =
    match e with
    | S.Ecst n -> D.val_cst n
    | Evar x -> C.read x aenv
    | Ebop (o, e0, e1) ->
        D.val_binop o (ai_expr e0 aenv) (ai_expr e1 aenv)

  (* ai_cond: cond -> nr_abs -> nr_abs  *)
  let ai_cond (r, x, n) (aenv: nr_abs) =
    let av = D.val_sat r n (C.read x aenv) in
    let open Poly in
    if av = D.val_bot then nr_bot aenv
    else C.write x av aenv

  (*
    〚skip〛P#(M#) = M#
    〚C₀;C₁〛P#(M#) = 〚C₁〛P#(〚C₀〛P#(M#))
    〚x:=E〛P#(M#) = M#[x ↦ 〚E〛#(M#)]
    〚input(x)〛P#(M#) = M#[x ↦ ⊤v]
    〚if(B){C₀}else{C₁}〛P#(M#) = 〚C₀〛P#(ℱB#(M#)) ⊔# 〚C₁〛P#(ℱ¬B#(M#))
    〚while(B){C}〛P#(M#) = ℱ¬B#(abs_iter(〚C〛P# ∘ ℱB#, M#))
  *)

  (* postlfp: (nr_abs -> nr_abs) -> nr_abs -> nr_abs *)
  let rec postlfp f a =
    let anext = f a in
    if nr_is_le anext a then a
    else postlfp f (nr_join a anext)

  (* Optional
      storage_init: unit -> unit
      storage_find: label -> nr_abehs
      storage_add: label -> nr_abs -> unit
  *)
  let storage = Hashtbl.create (module Int)
  let storage_init () = Hashtbl.clear storage
  let storage_find = Hashtbl.find_exn storage
  let storage_add l aenv =
    Hashtbl.add_exn storage ~key:l ~data:aenv

  (* ai_com: com -> nr_abs -> nr_abs *)
  let rec ai_com (l, c) aenv =
    (* Optional: accumulate all states *)
    storage_add l (nr_join (storage_find l) aenv);

    if nr_is_bot aenv then aenv
    else
      match c with
      | S.Cskip -> aenv
      | Cseq (c0, c1) -> ai_com c1 (ai_com c0 aenv)
      | Cassign (x, e) -> C.write x (ai_expr e aenv) aenv
      | Cinput x -> C.write x D.val_top aenv
      | Cif (b, c0, c1) ->
          nr_join
            (ai_com c0 (ai_cond b aenv))
            (ai_com c1 (ai_cond (S.cneg b) aenv))
      | Cwhile (b, c) ->
          let f_loop = fun a -> ai_com c (ai_cond b a) in
          ai_cond (S.cneg b) (postlfp f_loop aenv)


  (* ai_step: com -> label -> nr_abs -> (label * nr_abs) list *)
  let rec ai_step (_l, c) lnext aenv =
    match c with
    | S.Cskip -> [(lnext, aenv)]
    | Cseq (c0, c1) ->
        ai_step c0 (fst c1) aenv
    | Cassign (x, e) ->
        [(lnext, C.write x (ai_expr e aenv) aenv)]
    | Cinput x ->
        [(lnext, C.write x D.val_top aenv)]
    | Cif (b, c0, c1) ->
        [(fst c0, ai_cond b aenv);
         (fst c1, ai_cond (S.cneg b) aenv)]
    | Cwhile (b, c) ->
        [(fst c, ai_cond b aenv);
         (lnext, ai_cond (S.cneg b) aenv)]

  (* ai_iter: prog -> nr_abs -> unit *)
  (* let ai_iter p aenv =
    let (l, c) = first p in
    invs := I.add l aenv I.empty
    let wlist = T.create () in
    T.add l wlist;
    while not (T.is_empty wlist) do
      let l = T.pop wlist in
      let c = find p l in
      let lnext = next p l in
      let aenv = I.find l !invs in
      let aposts = ai_step (l, c) lnext in
      List.iter
        (fun (lnext, apost) ->
          let old_apost = I.find lnext !invs in
          if not (nr_is_le apost old_apost) then
            let new_apost = nr_join old_apost apost in
            invs := I.add lnext new_apost !invs;
            T.add lnext wlist
        ) aposts
    done *)
end