#lang racket/unit
(require
 (only-in racket/match match match-λ)
 (prefix-in r: racket/set)
 (only-in "set.rkt"       ∅ list→set)
 (only-in "nondet.rkt"    lift)
 "signatures.rkt"
 "terms.rkt")

;;;; reader & printer

(import (only syntax^    empty-ctx))
(export io^)

(define all-identifiers (r:mutable-set))

;; all-ids : → (SetM Id)
(define (all-ids)
  (lift (list→set (r:set->list all-identifiers))))

;; reader : Sexp → Stx
(define (reader sexp)
  (define read-stx
    (match-λ
     [(? prim?    p) (Stx p        (empty-ctx))]
     [(? boolean? b) (Stx (Bool b) (empty-ctx))]
     [(? real?    r) (Stx (Num  r) (empty-ctx))]
     [(? symbol?  s)
      (let ([id (Stx (Sym s) (empty-ctx))])
        (r:set-add! all-identifiers id)
        id)]
     [(? null?)      (Stx (Null)   (empty-ctx))]
     [(? pair?    p) (let ([stl (read-stl p)])
                       (match stl
                         [(Pair a d)   (Stx (Pair a d) (empty-ctx))]
                         [(? Stx? stx) stx]))]
     [x (error 'reader "not supported: ~a" x)]))
  (define read-stl
    (match-λ
     ['()            (Null)]
     [(cons a d)     (Pair (read-stx a) (read-stl d))]
     [(? Atom? atom) (read-stx atom)]))
  (r:set-clear! all-identifiers)
  (read-stx sexp))

(define (print-atom a)
  (match a
    [(Bool b)     b]
    [(Num  n)     n]
    [(Sym  nam)   nam]
    [(𝓁 nam)      nam]
    ;[(Defs scp 𝓁) '(Defs)]
    ))

(define (printer val)
  (match val
    [(? Atom? atom) (print-atom atom)]
    [(Null) '()]
    [(Pair a d) (cons (printer a) (printer d))]
    [(VFun `(,(Var nams) ...) _ast _env) `(VFun ,@nams)]
    [(Stx e _c) (vector 'stx (printer e))]))
