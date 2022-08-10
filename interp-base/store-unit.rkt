#lang racket/unit
(require
 racket/match
 (only-in "../term.rkt" use-terms)

 (only-in "../signatures.rkt" config^ store^)
 (only-in "../config.rkt" #%term-forms))

(import (only config^ Store%))
(export store^)

(use-terms Store)

;; ----------------------------------------
;; Store

; init-store : -> Store
(define (init-store) (Store 0 (make-immutable-hash)))

; lookup-store : Store Loc -> (U Val Cont)
(define (lookup-store st loc)
  (hash-ref (Store-tbl st) loc))

; update-store : Store Loc (U Val Cont) -> Store
(define (update-store st loc u)
  (Store (Store-size st)
         (hash-set (Store-tbl st) loc u)))

; update-store* : Store (Listof Loc) (Listof (U Val Cont)) -> Store
(define (update-store* st locs us)
  (Store (Store-size st)
         (foldl (λ (l u t) (hash-set t l u))
                (Store-tbl st) locs us)))

; alloc-loc : Symbol Store -> (Values Loc Store)
;   - called only from push-cont
;   - a unique lbl is generated for each App and If form during parse
(define (alloc-loc lbl st)
  (let ([size (Store-size st)])
    (values (string->symbol (format "~a::~a" lbl size))
            (Store (add1 size) (Store-tbl st)))))

; alloc-loc* : (Listof Nam) Store -> (Values (Listof Loc) Store)
;   for eval-time value binding
(define (alloc-loc* nams st)
  (match nams
    ['() (values '() st)]
    [(list nam1 nams ...)
     (let* ([size (Store-size st)]
            [loc_0 (string->symbol (format "~a:~a" nam1 size))])
       (let-values
           ([(locs_new store_new)
             (alloc-loc* nams (Store (add1 size) (Store-tbl st)))])
         (values (cons loc_0 locs_new) store_new)))]))
