#lang racket
(require "../set.rkt" "../dprint.rkt""../reduction.rkt"
         "../example.rkt"
         (only-in "../core/misc.rkt"
                  define-runner run-examples run-all-examples)
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
(define (evaluate ast)
  (call-with-values
   (λ () (eval 0 ast 'no-scope (init-ξ) (Σ* (init-Σ) (set) (set))))
   (λ (val Σ*) val)))

(define-runner run
  reader printer
  expander parser evaluate)

(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode)
  (run-examples run (append local:examples defs:examples) mode))

(define (run-all/runs core:run phases:run full:run)
  (let ([all-runs `([core 1 ,core:run]
                    [phases 2 ,phases:run]
                    [full 3 ,full:run])]
        [all-examples (list
                       core:examples
                       phases:examples
                       (append local:examples defs:examples))])
    (run-all-examples all-runs all-examples)))
(define run-all (run-all/runs core:run phases:run run))

;; for debug

;(: eval--> : Sexp -> (Setof State))
(define (eval--> form)
  (car (do ast <- (lift (run form 'parse))
           (lift (-->f `(,(AstEnv 0 ast (init-env) 'no-scope (init-ξ))
                         • ,(init-store) ,(Σ* (init-Σ) (set) (set))))))))

;(: eval-->* : Sexp -> (Setof State))
(define (eval-->* form #:steps [steps #f])
  (car (do ast <- (lift (run form 'parse))
           (lift (apply-reduction-relation*
                  -->f `(,(AstEnv 0 ast (init-env) 'no-scope (init-ξ))
                         • ,(init-store) ,(Σ* (init-Σ) (set) (set)))
                  #:steps steps)))))

;(: expand==> : Sexp -> (Setof ζ))
(define (expand==> form)
  (==>f
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (init-Θ) (Σ* (init-Σ) (set) (set)))))

;(: expand==>* : (->* (Sexp) (#:steps (Option Natural)) (Setof ζ)))
(define (expand==>* form #:steps [steps #f])
  (apply-reduction-relation*
   ==>f
   (ζ (Stxξ 0 (reader form) (init-ξ)) '∘ '• (init-Θ) (Σ* (init-Σ) (set) (set)))
   #:steps steps))
