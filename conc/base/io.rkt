#lang racket
(require "struct-sig.rkt"
         "syntax-sig.rkt")
(provide io^ io@)

;;;; reader & printer

(define-signature io^
  (reader printer))

(define-unit io@
  (import (only struct^ Var VFun Sym Stx stx stx? sym atom? prim?)
          (only syntax^ empty-ctx))
  (export io^)

  (define reader
    (letrec ([read-stx
              (λ (x)
                (match x
                  [(? prim?) (stx x (empty-ctx))]
                  [(? symbol?) (stx (sym x) (empty-ctx))]
                  [(? atom?) (stx x (empty-ctx))]
                  [(? pair?)
                   (let ([stl (read-stl x)])
                     (match stl
                       ['() (stx '() (empty-ctx))]
                       [(? stx? stx) stx]
                       [(? pair? stl) (stx stl (empty-ctx))]))]
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
  )

