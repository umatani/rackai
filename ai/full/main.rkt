#lang racket
(require "../../interp/dprint.rkt"
         "../../interp/full/struct.rkt"
         (only-in "../../interp/core/expand.rkt"
                  init-ξ init-Σ)
         (only-in "../../interp/phases/main.rkt"
                  reader printer)
         (only-in "../../interp/full/main.rkt"
                  expander/expand parser/parse main/runs
                  eval-->/--> eval-->*/--> expand==>/==> expand==>*/==>)
         (only-in "../core/main.rkt" [run core:run])
         (only-in "../phases/main.rkt" [run phases:run])

         ;; Abstract version
         (only-in "../core/misc.rkt"
                  define-runner run-examples run-all-examples)
         (only-in "../phases/parse.rkt" parse)
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

(define main (main/runs core:run phases:run run run-all-examples))

;; for debug

(define eval-->    (eval-->/-->    -->f))
(define eval-->*   (eval-->*/-->   -->f))
(define expand==>  (expand==>/==>  ==>f))
(define expand==>* (expand==>*/==> ==>f))
