#lang racket
(require "../dprint.rkt" "../reduction.rkt"
         "struct.rkt"
         "misc.rkt"
         (only-in "syntax.rkt" empty-ctx strip)
         (only-in "eval.rkt" init-env init-store -->c eval)
         (only-in "parse.rkt" parse)
         (only-in "expand.rkt" init-ξ init-Θ init-Σ ==>c expand)
         "../example.rkt"
         (for-syntax racket/list))
(provide (all-defined-out))

;(: expander : Stx -> (Values Stx Σ))
(define ((expander/expand expand) stx)
  (expand stx (init-ξ) (init-Σ)))
(define expander (expander/expand expand))

(define-helpers (empty-ctx) reader printer)

; (: stripper : Stx -> Val)
(define (stripper stx) (strip stx))

(define-runner run
  reader printer
  expander parse eval)

;; run example

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
