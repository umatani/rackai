#lang racket/unit
(require "../../base/set.rkt"
         "../../base/reduction.rkt"

         "../../base/struct-sig.rkt"
         (only-in "../../base/delta.rkt" delta^)
         "../../base/eval-sig.rkt"
         (only-in "../../base/core/eval-unit.rkt"
                  [-->/store base:-->/store]
                  ;init-env lookup-env update-env
                  ;init-store push-cont/alloc-loc/update-store
                  ))

(import struct^
        (prefix base: eval^))
(export)


;; Set-based heap

;(: lookup-store : Store Loc -> (SetM (U Val Cont))
(define (lookup-store store loc)
  (lift (hash-ref (Store-tbl store) loc)))

;(: update-store : Store Loc (U Val Cont) -> Store)
(define (update-store store loc u)
  (Store (Store-size store)
         (hash-update (Store-tbl store) loc (λ (old) (set-add old u)) (set))))

;(: update-store* : Store (Listof Loc) (Listof (U Val Cont)) -> Store)
(define (update-store* store locs us)
  (Store (Store-size store)
         (foldl (λ (loc u tbl)
                  (hash-update tbl loc (λ (old) (set-add old u)) (set)))
                (Store-tbl store) locs us)))


;; Finite-domain allocation

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


;; Revised reduction rules

;; (: --> : State -> (Setof State))
(define-parameterized-extended-reduction-relation (-->/store delta)
  (base:-->/store delta <-))

(define --> ((reducer-of -->/store) delta))

; (: eval : Ast -> (Setof Val))
(define ((eval/--> -->) ast)
  (match-let ([(set `(,(? Val? val) • ,_store) ...)
               (apply-reduction-relation*
                --> `(,(AstEnv ast (init-env)) • ,(init-store)))])
    (list->set val)))

(define eval (eval/--> -->))
