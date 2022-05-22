#lang racket
(require (only-in "../../interp/reduction.rkt"
                  reducer-of
                  define-parameterized-extended-reduction-relation)
         "../../interp/core/struct.rkt"
         "../../interp/core/delta.rkt"
         (only-in "../../interp/core/eval.rkt"
                  [-->c/store interp:-->c/store]
                  eval/-->
                  push-cont/alloc-loc/update-store
                  lookup-env update-env))
(provide (all-defined-out))

;; Set-based heap

;(: lookup-store : Store Loc -> (Setof (U Val Cont))
(define (lookup-store store loc)
  (hash-ref (Store-tbl store) loc))

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

;; (: -->c : State -> (Setof State))
(define-parameterized-extended-reduction-relation -->c/store interp:-->c/store
  (lookup-store update-store* alloc-loc* push-cont)

  ;; reference
  [`(,(AstEnv (? Var? var) env) ,cont ,store)
   #:with (lookup-store store (lookup-env env var))
   (λ (val) `(,(AstEnv val env) ,cont ,store))
   ev-x]

  ;; application
  [`(,(? Val? val) ,(KApp vals tms loc_cont) ,store)
   #:with (lookup-store store loc_cont)
   (λ (cont)`(,(SApp (append vals (list val)) tms) ,cont ,store))
   ev-pop-app]

  ;; if
  [`(,(? Val? val) ,(KIf tm_then tm_else loc_cont) ,store)
   #:with (lookup-store store loc_cont)
   (λ (cont) `(,(SIf val tm_then tm_else) ,cont ,store))
   ev-pop-if]

  )


(define -->c ((reducer-of -->c/store)
              lookup-store update-store* alloc-loc* push-cont))
(define eval (eval/--> -->c))
