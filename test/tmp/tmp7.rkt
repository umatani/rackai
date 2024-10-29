#lang racket
(require "../../reduction.rkt")

(define-reduction -->
  #:default [x (printf "default:\n") (+ x 100)]
  [1 1 one])

