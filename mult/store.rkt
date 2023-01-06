#lang racket
(require
 (except-in racket set do)
 "../set.rkt"
 "../nondet.rkt"
 "../mix.rkt"
 (only-in "../term.rkt" use-terms)

 (only-in "../signatures.rkt" store^)
 (only-in "../terms.rkt" #%term-forms
          Store%)
 (rename-in "../conc/units.rkt" [store@ base:store@]))
(provide store@)

(define-mixed-unit store@
  (import)
  (export store^)
  (inherit [base:store@ init-store alloc-loc alloc-loc*])
  (use-terms Store)

  ;; Set-based heap

  ; lookup-store : Store Loc -> (SetM (U Val Cont))
  (define (lookup-store store loc)
    (lift (hash-ref (Store-tbl store) loc)))

  ; update-store : Store Loc (U Val Cont) -> Store
  (define (update-store store0 loc u)
    (Store (Store-size store0)
           (hash-update (Store-tbl store0) loc
                        (λ (old) (set-add old u)) (set))))

  ; update-store* : Store (Listof Loc) (Listof (U Val Cont)) -> Store
  (define (update-store* store0 locs us)
    (Store (Store-size store0)
           (foldl (λ (loc u tbl)
                    (hash-update tbl loc (λ (old) (set-add old u)) (set)))
                  (Store-tbl store0) locs us))))