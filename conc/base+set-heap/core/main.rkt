#lang racket
(require "../../base/dprint.rkt"
         "../../base/nondet.rkt"
         "../../base/reduction.rkt"
         "../../base/core/struct.rkt"
         (only-in "../../base/example.rkt" core:examples)
         (only-in "../../base/core/misc.rkt" run-examples)
         (only-in "../../base/core/eval.rkt" init-env init-store)
         (only-in "../../base/core/expand.rkt" init-ξ init-Θ init-Σ)
         (only-in "../../base/core/main.rkt"
                  reader printer expander/expand)
         
         ;; Set-based version
         (only-in "misc.rkt" define-runner)
         (only-in "eval.rkt" -->c eval)
         (only-in "parse.rkt" parse)
         (only-in "expand.rkt" ==>c expand)
         
         (for-syntax racket/list))
(provide (all-defined-out))

(define expander (expander/expand expand))

(define-runner run
  reader printer
  expander parse eval)

(define (main [mode 'check])
  (run-examples run core:examples mode))

;; for debug

; (: eval--> : Sexp -> (Setof State))
(define (eval--> form)
  (car (do ast <- (lift (run form 'parse))
           (lift (-->c `(,(AstEnv ast (init-env)) • ,(init-store)))))))

; (: eval-->* : Sexp -> (Setof State))
(define (eval-->* form #:steps [steps #f])
  (car (do ast <- (lift (run form 'parse))
           (lift (apply-reduction-relation*
                  -->c `(,(AstEnv ast (init-env)) • ,(init-store))
                  #:steps steps)))))

;(: expand==> : Sexp -> (Setof ζ))
(define (expand==> form)
  (==>c (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Θ) (init-Σ))))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ)))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>c
   (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))
