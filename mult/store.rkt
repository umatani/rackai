#lang racket/base
(require
 racket/unit
 (only-in "../nondet.rkt"     lift)
 (only-in "../mix.rkt"        define-mixed-unit inherit)
 (only-in "../set.rkt"        ∅ set-add)
 "../signatures.rkt"
 "../terms.rkt"
 (only-in "../base/units.rkt" [store@ base:store@]))
(provide store@)

(define-mixed-unit store@
  (import)
  (export  store^)
  (inherit [base:store@ init-store alloc-loc alloc-loc*])

  ;;;; Set-based heap

  ;; lookup-store : Store Loc → (SetM (U Val Cont))
  (define (lookup-store sto loc)
    (lift (hash-ref (Store-tbl sto) loc)))

  ;; update-store : Store Loc (U Val Cont) → Store
  (define (update-store sto loc u)
    (Store (Store-size sto)
           (hash-update (Store-tbl sto) loc
                        (λ (us) (set-add us u)) ∅)))

  ;; update-store* : Store (Listof Loc) (Listof (U Val Cont)) → Store
  (define (update-store* sto locs us)
    (Store (Store-size sto)
           (foldl (λ (loc u tbl)
                    (hash-update tbl loc (λ (us) (set-add us u)) ∅))
                  (Store-tbl sto) locs us))))
