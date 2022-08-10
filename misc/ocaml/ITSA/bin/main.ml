open ITSA.Syntax
open ITSA.Conc

let com1 = (0, Cassign (0, Ecst 100))
let mem1 = Array.make 1 0

let mem1' = sem_com com1 mem1
let () = print_mem mem1'
let mem1'' = iter com1 mem1
let () = print_mem mem1''

let com2 =
   (0, Cseq ((1, Cassign (1, Ecst 1)),
             (2, Cwhile
                   ((Csup, 0, 0),
                    (3, Cseq ((4, Cassign (1, Ebop (Bmul, Evar 0, Evar 1))),
                              (5, Cassign (0, Ebop (Bsub, Evar 0, Ecst 1)))))))))
let mem2 = Array.of_list [10; -1]

let mem2' = sem_com com2 mem2
let () = print_mem mem2'
let mem2'' = iter com2 mem2
let () = print_mem mem2''

module Abs_cst = ITSA.Abs.Make(ITSA.Dom_cst)
module Abs_sign = ITSA.Abs.Make(ITSA.Dom_sign.Dom_sign)

(* let _nr1 = Abs_sign.ai_com com1 (Abs_sign.nr_init 1) *)

let () = Stdio.Out_channel.output_value Stdio.stdout ITSA.Dom_sign.Dom_sign.val_bot
