#lang racket
(require "../reduction.rkt"
         "../example.rkt"
         "../dprint.rkt"
         (only-in "../core/misc.rkt" define-runner run-all-examples)
         (only-in "../core/eval.rkt" init-env init-store)
         (only-in "../core/expand.rkt" init-ξ init-Θ init-Σ)
         (only-in "../core/main.rkt" [run core:run])
         (only-in "../phases/parse.rkt" parse)
         (only-in "../phases/main.rkt" reader printer [run phases:run])
         "struct.rkt"
         (only-in "expand-eval.rkt" -->f ==>f eval expand)
         (for-syntax racket/list))
(provide (all-defined-out))

;(: expander : Stx -> (Values Stx Σ*))
(define ((expander/expand expand) stx)
  (expand 0 stx (init-ξ) (Σ* (init-Σ) (set) (set))))
(define expander (expander/expand expand))

;(: parser : Stx Σ* -> Ast)
(define ((parser/parse parse) stx Σ*) (parse 0 stx (Σ*-Σ Σ*)))
(define parser (parser/parse parse))

;(: evaluate : Ast -> Val)
(define ((evaluate/eval eval) ast)
  (call-with-values
   (λ () (eval 0 ast 'no-scope (init-ξ) (Σ* (init-Σ) (set) (set))))
   (λ (val Σ*) val)))
(define evaluate (evaluate/eval eval))

(define-runner run
  reader printer
  expander parser evaluate)

#;
(define (main [mode : Symbol 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode)
  (run-examples run local:examples mode)
  (run-examples run defs:examples mode))

(define (main/runs core:run phases:run full:run)
  (let ([all-runs `([core ,core:run]
                    [phases ,phases:run]
                    [full ,full:run])]
        [all-examples (list
                       core:examples
                       phases:examples
                       (append local:examples defs:examples))])
    (run-all-examples all-runs all-examples)))
(define main (main/runs core:run phases:run run))

;; for debug

;(: eval--> : Sexp -> (Setof State))
(define ((eval-->/--> -->) form)
  (--> `(,(AstEnv 0 (run form 'parse)
                  (init-env) 'no-scope (init-ξ))
         • ,(init-store) ,(Σ* (init-Σ) (set) (set)))))
(define eval--> (eval-->/--> -->f))

;(: eval-->* : Sexp -> (Listof State))
(define ((eval-->*/--> -->) form)
  (apply-reduction-relation*
   -->
   `(,(AstEnv 0 (run form 'parse)
              (init-env) 'no-scope (init-ξ))
     • ,(init-store) ,(Σ* (init-Σ) (set) (set)))))
(define eval-->* (eval-->*/--> -->f))

;(: expand==> : Sexp -> (Setof ζ))
(define ((expand==>/==> ==>) form)
  (==>
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (init-Θ) (Σ* (init-Σ) (set) (set)))))
(define expand==> (expand==>/==> ==>f))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Listof ζ)))
(define ((expand==>*/==> ==>) form #:steps [steps #f])
  (apply-reduction-relation*
   ==>
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (init-Θ) (Σ* (init-Σ) (set) (set)))
   #:steps steps))
(define expand==>* (expand==>*/==> ==>f))
