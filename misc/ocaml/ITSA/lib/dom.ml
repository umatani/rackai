module type M = sig
  type t
  val val_bot : t
  val val_top : t
  val val_incl : t -> t -> bool
  val val_cst : Syntax.const -> t
  val val_sat : Syntax.rel -> int -> t -> t
  val val_join : t -> t -> t
  val val_binop : Syntax.bop -> t -> t -> t
end
