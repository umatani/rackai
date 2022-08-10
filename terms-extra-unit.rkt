#lang racket/unit
(require
 (for-syntax racket)
 (only-in "term.rkt" use-terms)

 (only-in "signatures.rkt" terms-extra^ config^)
 (only-in "terms.rkt"
          Val% Atom% Stx% Null% Pair% Hole%
          prim?
          [#%term-forms tm:#%term-forms])
 (only-in "config.rkt" [#%term-forms cfg:#%term-forms]))

(import (only config^ Stxξ%))
(export terms-extra^)

(define-syntax #%term-forms
  (append (syntax-local-value #'tm:#%term-forms)
          (syntax-local-value #'cfg:#%term-forms)))
(use-terms Val Atom Stx Null Pair Hole Stxξ)

(define (val? x)
  (or (Val? x)
      (and (Pair? x) (val? (Pair-a x)) (val? (Pair-d x)))
      (stx? x)))

(define (stx? x)
  (or (and (Stx? x) (Atom? (Stx-e x)))
      (and (Stx? x) (prim? (Stx-e x)))
      (and (Stx? x) (Pair? (Stx-e x))
           (stx? (Pair-a (Stx-e x)))
           (stl? (Pair-d (Stx-e x))))
      (and (Stx? x) (proper-stl? (Stx-e x)))
      (Stxξ? x)
      (Hole? x)
      (and (Stx? x) (Hole? (Stx-e x)))))

(define (stl? x)
  (or (Null? x) (stx? x)
      (and (Pair? x) (stx? (Pair-a x)) (stl? (Pair-d x)))
      (Hole? x)))

(define (proper-stl? x)
  (or (Null? x)
      (and (Pair? x) (stx? (Pair-a x)) (proper-stl? (Pair-d x)))))
