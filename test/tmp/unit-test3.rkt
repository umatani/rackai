#lang racket
(require (submod "unit-test2.rkt" experiment0))

(define-unit =>>@ (import ->>^) (export)
  (define Y (+ X 1))
  Y)

(invoke-unit (compound-unit
              (import) (export)
              (link (([s : ->>^]) ->>@)
                    (() =>>@ s))))
