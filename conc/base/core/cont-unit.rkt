#lang racket/unit
(require "../../../store-sig.rkt"
         "../../../cont-sig.rkt")

(import (only store^
              alloc-loc update-store))
(export cont^)

;(: push-cont : Store Cont -> (Values Loc Store))
(define (push-cont st cont)
  (let-values ([(loc store_1) (alloc-loc st)])
    (let ([store_2 (update-store store_1 loc cont)])
      (values loc store_2))))
