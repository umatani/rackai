#lang racket/unit
(require
 racket/match
 (only-in "../../../term.rkt" use-terms)
 
 (only-in "../../../signatures.rkt"
          terms-extra^ syntax^ io^)
 (only-in "terms.rkt" terms^ #%term-forms))

;;;; reader & printer

(import (only terms^
              Var% VFun% Stx% Sym%)
        (only terms-extra^
              stx? atom? prim?)
        (only syntax^
              empty-ctx))
(export io^)

(use-terms Var VFun Stx Sym)

(define reader
  (letrec ([read-stx
            (λ (x)
              (match x
                [(? prim?) (Stx x (empty-ctx))]
                [(? symbol?) (Stx (Sym x) (empty-ctx))]
                [(? atom?) (Stx x (empty-ctx))]
                [(? pair?)
                 (let ([stl (read-stl x)])
                   (match stl
                     ['() (Stx '() (empty-ctx))]
                     [(? stx? stx) stx]
                     [(? pair? stl) (Stx stl (empty-ctx))]))]
                [_ (error 'reader "not supported: ~a" x)]))]
           [read-stl
            (λ (xs)
              (match xs
                ['() '()]
                [(cons y ys)
                 (cons (read-stx y) (read-stl ys))]
                [(? atom? atom) (read-stx atom)]))])
    read-stx))

(define (printer val)
  (match val
    [(? (λ (v) (or (null? v) (prim? v) (real? v) (boolean? v)))) val]
    [(VFun `(,(Var nams) ...) ast env) `(VFun ,@nams)]
    [(Sym nam) nam]
    [(cons v1 v2) (cons (printer v1) (printer v2))]
    [(Stx a c) (vector 'stx (printer a))]
    ;[(𝓁 nam) nam]
    ;[(Defs _ _) '(Defs)]
    ))
