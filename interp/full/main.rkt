#lang racket
(require "../reduction.rkt"
         "../example.rkt"
         (only-in "../core/conf.rkt" init-env init-store init-ξ init-Θ init-Σ)
         (only-in "../core/misc.rkt" define-runner run-all-examples)
         (only-in "../core/main.rkt" [run core:run])
         (only-in "../phases/parse.rkt" parse)
         (only-in "../phases/main.rkt" reader printer [run phases:run])
         "struct.rkt"
         (only-in "expand-eval.rkt" -->f ==>f eval expand)
         (for-syntax racket/list))

;(: expander : Stx -> (Values Stx Σ*))
(define (expander stx)
  (expand 0 stx (init-ξ) (Σ* (init-Σ) (set) (set))))

;(: parser : Stx Σ* -> Ast)
(define (parser stx Σ*) (parse 0 stx (Σ*-Σ Σ*)))

;(: evaluate : Ast -> Val)
(define (evaluate ast)
  (call-with-values
   (λ () (eval 0 ast 'no-scope (init-ξ) (Σ* (init-Σ) (set) (set))))
   (λ (val Σ*) val)))

(define-runner run
  reader
  expander
  stripper printer
  evaluate
  parser)

#;
(define (main [mode : Symbol 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode)
  (run-examples run local:examples mode)
  (run-examples run defs:examples mode))

(define main
  (let ([all-runs `([core ,core:run]
                    [phases ,phases:run]
                    [full ,run])]
        [all-examples (list
                       core:examples
                       phases:examples
                       (append local:examples defs:examples))])
    (run-all-examples all-runs all-examples)))


;; for debug

;(: eval--> : Sexp -> (Setof State))
(define (eval--> form)
  ((reducer-of -->f)
   `(,(AstEnv 0 (run form 'parse)
              (init-env) 'no-scope (init-ξ))
     • ,(init-store) ,(Σ* (init-Σ) (set) (set)))))

;(: eval-->* : Sexp -> (Listof State))
(define (eval-->* form)
  (apply-reduction-relation*
   (reducer-of -->f)
   `(,(AstEnv 0 (run form 'parse)
              (init-env) 'no-scope (init-ξ))
     • ,(init-store) ,(Σ* (init-Σ) (set) (set)))))

;(: expand==> : Sexp -> (Setof ζ))
(define (expand==> form)
  ((reducer-of ==>f)
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (init-Θ) (Σ* (init-Σ) (set) (set)))))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Listof ζ)))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   (reducer-of ==>f)
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (init-Θ) (Σ* (init-Σ) (set) (set)))
   #:steps steps))
