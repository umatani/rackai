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
