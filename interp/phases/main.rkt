#lang racket
(require "../set.rkt" "../dprint.rkt" "../reduction.rkt"
         "../example.rkt"
         (only-in "../core/misc.rkt"
                  define-helpers define-runner run-examples)
         (only-in "../core/eval.rkt" init-env init-store -->c eval)
         (only-in "../core/expand.rkt" init-ξ init-Θ init-Σ)
         "struct.rkt"
         (only-in "syntax.rkt" empty-ctx)
         (only-in "parse.rkt" parse)
         (only-in "expand.rkt" ==>p expand)
         (for-syntax racket/list))
(provide (all-defined-out))

(define-helpers (empty-ctx) reader printer)

;(: expander : Stx -> (Values Stx Σ))
(define ((expander/expand expand) stx)
  (expand 0 stx (init-ξ) (set) (init-Σ)))
(define expander (expander/expand expand))

;(: parser : Stx Σ -> Ast)
(define ((parser/parse parse) stx Σ) (parse 0 stx Σ))
(define parser (parser/parse parse))

(define-runner run
  reader printer
  expander parser eval)

(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode))

;; for debug

; (: eval--> : Sexp -> (Setof State))
(define ((eval-->/--> -->) form)
  (--> `(,(AstEnv (run form 'parse) (init-env)) • ,(init-store))))
(define eval--> (eval-->/--> -->c))

; (: eval-->* : Sexp -> (Listof State))
(define ((eval-->*/--> -->) form)
  (apply-reduction-relation*
   -->
   `(,(AstEnv (run form 'parse) (init-env)) • ,(init-store))))
(define eval-->* (eval-->*/--> -->c))

;(: expand==> : Sexp -> (Setof ζ))
(define ((expand==>/==> ==>) form)
  (==> (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Θ) (init-Σ))))
(define expand==> (expand==>/==> ==>p))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Listof ζ)))
(define ((expand==>*/==> ==>) form #:steps [steps #f])
  (apply-reduction-relation*
   ==>
   (ζ (Stxξ 0 (reader form) (init-ξ) (set)) '∘ '• (init-Θ) (init-Σ))
   #:steps steps))
(define expand==>* (expand==>*/==> ==>p))
