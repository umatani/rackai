#lang racket/unit
(require
 (only-in racket/match match)
 "../signatures.rkt"
 "../terms.rkt")

(import)
(export store^)

;; ----------------------------------------
;; Store

;; init-store : → Store
(define (init-store) (Store 0 (make-immutable-hash)))

;; lookup-store : Store Loc → (U Val Cont)
(define (lookup-store sto loc)
  (hash-ref (Store-tbl sto) loc))

;; update-store : Store Loc (U Val Cont) → Store
(define (update-store sto loc u)
  (Store (Store-size sto) (hash-set (Store-tbl sto) loc u)))

;; lookup-cont : Store Loc → Cont
(define (lookup-cont sto loc)
  (lookup-store sto loc))

;; lookup-val : Store Loc → Val
(define (lookup-val sto loc)
  (lookup-store sto loc))

;; alloc-loc : Nam Store → (Values Loc Store)
;;   - called via alloc-loc*.
;;   - also called directly from push-cont.
;;     Unique lbl is generated for each App and If during parse.
(define (alloc-loc lbl sto)
  (let ([size (Store-size sto)])
    (values (string->symbol (format "~a::~a" lbl size))
            (Store (add1 size) (Store-tbl sto)))))
