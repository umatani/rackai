#lang racket
(require "../../base/dprint.rkt"
         "../../base/reduction.rkt"
         (only-in "../../base/example.rkt" core:examples phases:examples)
         (only-in "../../base/core/misc.rkt" run-examples)
         (only-in "../../base/core/eval.rkt" init-env init-store)
         (only-in "../../base/core/expand.rkt" init-ξ init-Θ init-Σ)
         "../../base/phases/struct.rkt"
         (only-in "../../base/phases/main.rkt"
                  reader printer expander/expand parser/parse)

         ;; Set-based version
         (only-in "../core/misc.rkt" define-runner)
         (only-in "../core/eval.rkt" -->c eval)
         (only-in "parse.rkt" parse)
         (only-in "expand.rkt" ==>p expand)

         (for-syntax racket/list))
(provide (all-defined-out))

(define expander (expander/expand expand))
(define parser (parser/parse parse))

(define-runner run
  reader printer
  expander parser eval)

(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode))

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
  (==>p (ζ (Stxξ 0 (reader form) (init-ξ) (set))
            '∘ '• (init-Θ) (init-Σ))))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ)))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>p
   (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))
