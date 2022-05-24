#lang racket
(require "../reduction.rkt"
         "../dprint.rkt"
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

(define ((main/run run) [mode 'check])
  (run-examples run core:examples mode))
(define main (main/run run))


;; for debug

; (: eval--> : Sexp -> (Setof State))
(define ((eval-->/--> -->) form)
  (-->
   `(,(AstEnv (run form 'parse) (init-env)) • ,(init-store))))
(define eval--> (eval-->/--> -->c))

; (: eval-->* : Sexp -> (Listof State))
(define ((eval-->*/--> -->) form)
  (apply-reduction-relation*
   -->
   `(,(AstEnv (run form 'parse) (init-env)) • ,(init-store))))
(define eval-->* (eval-->*/--> -->c))

;(: expand==> : Sexp -> (Setof ζ))
(define ((expand==>/==> ==>) form)
  (==> (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Θ) (init-Σ))))
(define expand==> (expand==>/==> ==>c))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Listof ζ)))
(define ((expand==>*/==> ==>) form #:steps [steps #f])
  (apply-reduction-relation*
   ==>
   (ζ (Stxξ (reader form) (init-ξ)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))
(define expand==>* (expand==>*/==> ==>c))
