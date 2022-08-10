module S = Syntax
module Make :
  functor (D : Dom.M) ->
    sig
      type nr_abs
      val nr_init : int -> nr_abs
      val nr_bot : D.t array -> D.t array
      val nr_is_bot : D.t array -> bool
      val nr_is_le : nr_abs -> nr_abs -> bool
      val nr_join : nr_abs -> nr_abs -> nr_abs
      val ai_expr : S.expr -> nr_abs -> D.t
      val ai_cond : S.rel * S.var * S.const -> nr_abs -> nr_abs
      val postlfp : (nr_abs -> nr_abs) -> nr_abs -> nr_abs
      val storage_init : unit -> unit
      val storage_find : int -> nr_abs
      val storage_add : int -> nr_abs -> unit
      val ai_com : S.com -> nr_abs -> nr_abs
      val ai_step : S.com -> S.label -> nr_abs -> (S.label * nr_abs) list
    end
