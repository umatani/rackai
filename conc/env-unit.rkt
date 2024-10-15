#lang racket/unit
(require
 "../signatures.rkt")

(import)
(export env^)

;; ----------------------------------------
;; Environment:

; init-env : → Env
(define (init-env) (make-immutable-hash))

; lookup-env : Env Var → Loc
(define (lookup-env env var) (hash-ref env var))

; extend-env* : Env (Listof Var) (Listof Loc) → Env
(define (extend-env* env vars locs)
  (foldl (λ (v l e) (hash-set e v l))
         env vars locs))
