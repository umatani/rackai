#lang racket/unit
(require
 "../signatures.rkt")

(import (only store^    alloc-loc update-store))
(export cont^)

;; ----------------------------------------
;; Continuation:

; push-cont : Store Label Cont → (Values Loc Store)
;   - lbl is generated for each AST (currently for App and If) during parse
(define (push-cont sto lbl cnt)
  (let-values ([(loc sto′) (alloc-loc lbl sto)])
    (values loc (update-store sto′ loc cnt))))
