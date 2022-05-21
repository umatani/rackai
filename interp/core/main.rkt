#lang racket
(require "../reduction.rkt"
         "struct.rkt"
         "misc.rkt"
         (only-in "syntax.rkt" strip)
         (only-in "conf.rkt" empty-ctx init-env init-store
                  init-ξ init-Θ init-Σ)
         (only-in "eval.rkt" -->c eval)
         (only-in "parse.rkt" parse)
         (only-in "expand.rkt" ==>c expand)
         "../example.rkt"
         (for-syntax racket/list))
(provide (all-defined-out))

;(: expander : Stx -> (Values Stx Σ))
(define (expander stx)
  (expand stx (init-ξ) (init-Σ)))


(define-helpers (empty-ctx) reader printer)

; (: stripper : Stx -> Val)
(define (stripper stx) (strip stx))

(define-runner run
  reader
  expander
  stripper printer
  eval
  parse)


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
