#lang racket/unit
(require racket/match
         "../../../dprint.rkt"

         "../../../struct-sig.rkt"
         "../../../store-sig.rkt")

(import (only struct^
              store Store-tbl Store-size))
(export store^)


;; ----------------------------------------
;; Store

; (: init-store : -> Store)
(define (init-store) (store 0 (make-immutable-hash)))

;(: lookup-store : Store Loc -> (U Val Cont))
(define (lookup-store st loc)
  (dprint 'core 'lookup-store "")
  (hash-ref (Store-tbl st) loc))

;(: update-store : Store Loc (U Val Cont) -> Store)
(define (update-store st loc u)
  (dprint 'core 'update-store "")
  (store (Store-size st)
         (hash-set (Store-tbl st) loc u)))

;(: update-store* : Store (Listof Loc) (Listof (U Val Cont)) -> Store)
(define (update-store* st locs us)
  (dprint 'core 'update-store* "")
  (store (Store-size st)
         (foldl (λ (l u t) (hash-set t l u))
                (Store-tbl st) locs us)))

;(: alloc-loc : Store -> (Values Loc Store))
(define (alloc-loc st)
  (dprint 'core 'alloc-loc "")
  (let ([size (Store-size st)])
    (values (string->symbol (format "l~a" size))
            (store (add1 size) (Store-tbl st)))))

;; for eval-time value binding
;(: alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store))
(define (alloc-loc* nams st)
  (dprint 'core 'alloc-loc* "")
  (match nams
    ['() (values '() st)]
    [(list nam1 nams ...)
     (let* ([size (Store-size st)]
            [loc_0 (string->symbol (format "~a:~a" nam1 size))])
       (let-values ([(locs_new store_new)
                     (alloc-loc* nams (store (add1 size) (Store-tbl st)))])
         (values (cons loc_0 locs_new) store_new)))]))
