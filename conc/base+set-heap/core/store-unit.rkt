#lang racket/unit
(require (except-in racket set do)
         "../../base/set.rkt" "../../base/nondet.rkt"
         "../../base/struct-sig.rkt"
         "../../base/store-sig.rkt")

(import (only struct^
              store Store-size Store-tbl)
        (prefix base: (only store^ init-store)))
(export store^)

(define init-store    base:init-store)


;; Set-based heap

;(: lookup-store : Store Loc -> (SetM (U Val Cont))
(define (lookup-store store loc)
  (lift (hash-ref (Store-tbl store) loc)))

;(: update-store : Store Loc (U Val Cont) -> Store)
(define (update-store store0 loc u)
  (store (Store-size store0)
         (hash-update (Store-tbl store0) loc (λ (old) (set-add old u)) (set))))

;(: update-store* : Store (Listof Loc) (Listof (U Val Cont)) -> Store)
(define (update-store* store0 locs us)
  (store (Store-size store0)
         (foldl (λ (loc u tbl)
                  (hash-update tbl loc (λ (old) (set-add old u)) (set)))
                (Store-tbl store0) locs us)))


;; Finite-domain allocation

;(: alloc-loc : Store -> (Values Loc Store))
(define (alloc-loc store0)
  (let ([size (Store-size store0)])
    (values (string->symbol (format "l~a" size))
            (store (add1 size) (Store-tbl store0)))))

;; for eval-time value binding
;(: alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store))
(define (alloc-loc* nams store0)
  (match nams
    ['() (values '() store0)]
    [(list nam1 nams ...)
     (let* ([size (Store-size store0)]
            [loc_0 (string->symbol (format "~a:~a" nam1 size))])
       (let-values ([(locs_new store_new)
                     (alloc-loc* nams (store (add1 size) (Store-tbl store0)))])
         (values (cons loc_0 locs_new) store_new)))]))
