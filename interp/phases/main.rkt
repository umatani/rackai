#lang racket
(require "../reduction.rkt"
         "../example.rkt"
         (only-in "../core/misc.rkt"
                  define-helpers define-runner run-examples)
         (only-in "../core/conf.rkt"
                  init-env init-store init-ξ init-Σ init-Θ)
         (only-in "../core/eval.rkt" -->c eval)
         "struct.rkt"
         (only-in "conf.rkt" empty-ctx)
         (only-in "parse.rkt" parse)
         (only-in "expand.rkt" ==>p expand)
         (for-syntax racket/list))
(provide reader printer run)

(define-helpers (empty-ctx) reader printer)

;(: expander : Stx -> (Values Stx Σ))
(define (expander stx)
  (expand 0 stx (init-ξ) (set) (init-Σ)))

;(: parser : Stx Σ -> Ast)
(define (parser stx Σ) (parse 0 stx Σ))

(define-runner run
  reader
  expander
  stripper printer
  eval
  parser)


(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode))


;; for debug

; (: eval--> : Sexp -> (Setof State))
(define (eval--> form)
  ((reducer-of -->c)
   `(,(AstEnv (run form 'parse) (init-env)) • ,(init-store))))

; (: eval-->* : Sexp -> (Listof State))
(define (eval-->* form)
  (apply-reduction-relation*
   (reducer-of -->c)
   `(,(AstEnv (run form 'parse) (init-env)) • ,(init-store))))

;(: expand==> : Sexp -> (Setof ζ))
(define (expand==> form)
  ((reducer-of ==>p)
   (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Θ) (init-Σ))))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Listof ζ)))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   (reducer-of ==>p)
   (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))
