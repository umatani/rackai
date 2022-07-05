#lang racket/unit
(require
 (only-in "../../signatures.rkt" cont^ store^))

(import (only store^ alloc-loc update-store))
(export cont^)


;; ----------------------------------------
;; Continuation:

; push-cont : Store Cont -> (Values Loc Store)
(define (push-cont st cont)
  (let-values ([(loc st_1) (alloc-loc st)])
    (values loc (update-store st_1 loc cont))))
