#lang racket/unit
(require
 racket/match
 (only-in "term.rkt" use-terms)

 (only-in "prim.rkt" prim?)
 (only-in "signatures.rkt" terms-extra^ syntax^ io^)
 (only-in "terms.rkt" terms^ #%term-forms))

;;;; reader & printer

(import (only terms^
              Var% VFun% Atom% Stx% Bool% Num% Sym% Null% Pair%
              Defs% 𝓁%)
        (only terms-extra^
              stx?)
        (only syntax^
              empty-ctx))
(export io^)

(use-terms Var VFun Atom Stx Bool Num Sym Null Pair Defs 𝓁)

(define reader
  (letrec
      ([read-stx
        (λ (x)
          (match x
            [(? prim?)    (Stx x        (empty-ctx))]
            [(? null?)    (Stx (Null)   (empty-ctx))]
            [(? boolean?) (Stx (Bool x) (empty-ctx))]
            [(? real?)    (Stx (Num  x) (empty-ctx))]
            [(? symbol?)  (Stx (Sym  x) (empty-ctx))]
            [(? pair?)    
             (let ([stl (read-stl x)])
               (match stl
                 [(Null)       (Stx (Null) (empty-ctx))]
                 [(Pair a d)   (Stx (Pair a d) (empty-ctx))]
                 [(? stx? stx) stx]))]
            [_ (error 'reader "not supported: ~a" x)]))]
       [read-stl
        (λ (xs)
          (match xs
            ['()            (Null)]
            [(cons y ys)    (Pair (read-stx y) (read-stl ys))]
            [(? Atom? atom) (read-stx atom)]))])
    read-stx))

(define (print-atom a)
  (match a
    [(Null)     '()]
    [(Bool b)   b]
    [(Num  n)   n]
    [(Sym  nam) nam]
    [(𝓁 nam) nam]
    [(Defs scp 𝓁) '(Defs)]))

(define (printer val)
  (match val
    [(Null) '()]
    [(? Atom? atom) (print-atom atom)]
    [(Pair v1 v2) (cons (printer v1) (printer v2))]
    [(VFun `(,(Var nams) ...) ast env) `(VFun ,@nams)]
    [(Stx a c) (vector 'stx (printer a))]))
