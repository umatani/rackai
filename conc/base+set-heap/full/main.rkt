#lang racket
(require "../../base/dprint.rkt"
         "../../base/reduction.rkt"
         (only-in "../../base/example.rkt"
                  core:examples phases:examples local:examples defs:examples)
         (only-in "../../base/core/misc.rkt" run-examples run-all-examples)
         (only-in "../../base/core/eval.rkt" init-env init-store)
         (only-in "../../base/core/expand.rkt" init-ξ init-Θ init-Σ)
         (only-in "../../base/phases/main.rkt" reader printer)
         "../../base/full/struct.rkt"
         (only-in "../../base/full/main.rkt"
                  expander/expand parser/parse run-all/runs)

         ;; Set-based version
         (only-in "../core/main.rkt" [run core:run])
         (only-in "../core/misc.rkt" define-runner)
         (only-in "../phases/parse.rkt" parse)
         (only-in "../phases/main.rkt" [run phases:run])
         (only-in "expand-eval.rkt" -->f eval ==>f expand)

         (for-syntax racket/list))
(provide (all-defined-out))

;(: evaluate : Ast -> (Setof Val))
(define ((evaluate/eval eval) ast)
  (for/set ([val+Σ*
             (in-set (eval 0 ast 'no-scope (init-ξ)
                           (Σ* (init-Σ) (set) (set))))])
    (car val+Σ*)))
(define evaluate (evaluate/eval eval))

(define parser (parser/parse parse))
(define expander (expander/expand expand))

(define-runner run
  reader printer
  expander parser evaluate)

(define (main [mode 'check])
  (run-examples run core:examples mode)
  (run-examples run phases:examples mode)
  (run-examples run (append local:examples defs:examples) mode))

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
