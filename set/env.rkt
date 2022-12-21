#lang racket
(require
 "../nondet.rkt"
 "../mix.rkt"

 (only-in "../signatures.rkt" env^)
 (only-in "../interp-base/units.rkt" [env@ base:env@]))
(provide env@)

(define-mixed-unit env@
  (import)
  (export env^)
  (inherit [base:env@ init-env])

  ;; Set-based Environment

  ; lookup-env : Env Var -> (SetM Loc)
  (define (lookup-env env var)
    (lift (hash-ref env var (λ () (set)))))

  ; extend-env : Env (Listof Var) (Listof Loc) -> Env
  (define (extend-env env vars locs)
    (foldl (λ (v l e)
             (hash-update e v (λ (old) (set-add old l)) (set)))
           env vars locs)))
