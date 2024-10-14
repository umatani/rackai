#lang racket/unit
(require
 "../signatures.rkt")

(import (only store^    alloc-loc update-store))
(export cont^)


;; ----------------------------------------
;; Continuation:

; push-cont : Store Label Cont -> (Values Loc Store)
;   lbl is generated for each AST (currently for App and If)
(define (push-cont st lbl cont)
  (let-values ([(loc st_1) (alloc-loc lbl st)])
    (values loc (update-store st_1 loc cont))))
