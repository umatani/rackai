#lang racket/base
(require
 racket/unit
 (only-in "../nondet.rkt"     lift)
 (only-in "../mix.rkt"        define-mixed-unit inherit)
 (only-in "../set.rkt"        ∅ set-add)
 "../signatures.rkt"
 (only-in "../base/units.rkt" [env@ base:env@]))
(provide env@)

(define-mixed-unit env@
  (import)
  (export  env^)
  (inherit [base:env@ init-env])

  ;; Set-based Environment

  ; lookup-env : Env Var -> (SetM Loc)
  (define (lookup-env env var)
    (lift (hash-ref env var ∅)))

  ; extend-env* : Env (Listof Var) (Listof Loc) -> Env
  (define (extend-env* env vars locs)
    (foldl (λ (v l e)
             (hash-update e v (λ (old) (set-add old l)) ∅))
           env vars locs)))
