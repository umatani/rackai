#lang racket
(require (only-in "../../interp/reduction.rkt" reducer-of)
         "../../interp/core/struct.rkt"
         (only-in "../../interp/core/eval.rkt" -->c/store eval/-->
                  push-cont/alloc-loc/update-store))
(provide (all-defined-out))

;(: lookup-store : Store Loc -> (U Val Cont))
(define (lookup-store store loc)
  (hash-ref (Store-tbl store) loc))

;(: update-store : Store Loc (U Val Cont) -> Store)
(define (update-store store loc u)
  (Store (Store-size store)
         (hash-set (Store-tbl store) loc u)))

;(: update-store* : Store (Listof Loc) (Listof (U Val Cont)) -> Store)
(define (update-store* store locs us)
  (Store (Store-size store)
         (foldl (λ (l u t) (hash-set t l u))
                (Store-tbl store) locs us)))

;(: alloc-loc : Store -> (Values Loc Store))
(define (alloc-loc store)
  (let ([size (Store-size store)])
    (values (string->symbol (format "l~a" size))
            (Store (add1 size) (Store-tbl store)))))

;; for eval-time value binding
;(: alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store))
(define (alloc-loc* nams store)
  (match nams
    ['() (values '() store)]
    [(list nam1 nams ...)
     (let* ([size (Store-size store)]
            [loc_0 (string->symbol (format "~a:~a" nam1 size))])
       (let-values ([(locs_new store_new)
                     (alloc-loc* nams (Store (add1 size) (Store-tbl store)))])
         (values (cons loc_0 locs_new) store_new)))]))

(define push-cont (push-cont/alloc-loc/update-store alloc-loc update-store))

(define -->c ((reducer-of -->c/store)
              lookup-store update-store* alloc-loc* push-cont))

(define eval (eval/--> -->c))
