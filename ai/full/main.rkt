#lang racket
(require "../../interp/dprint.rkt"
         (only-in "../../interp/core/misc.rkt" define-runner)
         (only-in "../../interp/phases/main.rkt"
                  reader printer)
         (only-in "../../interp/full/main.rkt"
                  evaluate/eval expander/expand parser/parse main/runs
                  eval-->/--> eval-->*/--> expand==>/==> expand==>*/==>)
         (only-in "../core/main.rkt" [run core:run])
         (only-in "../phases/main.rkt" [run phases:run])

         ;; Abstract version
         (only-in "../phases/parse.rkt" parse)
         (only-in "expand-eval.rkt" -->f eval ==>f expand)

         (for-syntax racket/list))

(define evaluate (evaluate/eval eval))
(define parser (parser/parse parse))
(define expander (expander/expand expand))

(define-runner run
  reader
  expander
  stripper printer
  evaluate
  parser)

(define main (main/runs core:run phases:run run))

;; for debug

(define eval-->    (eval-->/-->    -->f))
(define eval-->*   (eval-->*/-->   -->f))
(define expand==>  (expand==>/==>  ==>f))
(define expand==>* (expand==>*/==> ==>f))
