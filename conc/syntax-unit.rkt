#lang racket/unit
(require
 (only-in racket/match match match*)
 (only-in "../set.rkt" set-member? set-add set-remove set-union set-subtract)
 "../signatures.rkt"
 "../terms.rkt")

(import (only domain^    stx?))
(export syntax^)

;; ----------------------------------------
;; Syntax-object operations:

(define (empty-ctx . args) (error "must not be used"))

; zip : ProperStl ProperStl Ctx -> ProperStl
(define (zip stl_1 stl_2 ctx)
  (match* (stl_1 stl_2)
    [((Null) (Null)) (Null)]
    [((Pair stx_left stl_lefts) (Pair stx_right stl_rights))
     (Pair (Stx (Pair stx_left (Pair stx_right (Null))) ctx)
           (zip stl_lefts stl_rights ctx))]))

; unzip : ProperStl -> (Values ProperStl ProperStl)
(define (unzip stl)
  (match stl
    [(Null) (values (Null) (Null))]
    [(Pair (Stx (Pair stx_left (Pair stx_right (Null))) _) stl_rest)
     (let-values ([(stl_lefts stl_rights) (unzip stl_rest)])
       (values (Pair stx_left  stl_lefts)
               (Pair stx_right stl_rights)))]))

; in-hole : Stx Stx -> Stx
(define (in-hole . args) (error "must not be used"))

; in-hole-stl : Stl Stx -> Stl
(define (in-hole-stl in-hole stl v)
  (match stl
    [(? stx? stx) (in-hole stx v)]
    [(Pair stx stl) (Pair (in-hole stx v) (in-hole-stl in-hole stl v))]
    [(Hole) v]
    [_ stl]))

;; Recursively strips lexical context from a syntax object
; strip : Stl -> Val
(define (strip stl)
  (match stl
    [(Null) (Null)]
    [(Stx (Pair stx stl) _) (Pair (strip stx) (strip stl))]
    [(Stx x _) x]
    [(Pair stx stl) (Pair (strip stx) (strip stl))]))

;; Adds or cancels a scope
; addremove : Scp Scps -> Scps
(define (addremove scp scps)
  (if (set-member? scps scp)
      (set-remove scps scp)
      (set-add scps scp)))

; subtract : Scps Scps -> Scps
(define (subtract scps1 scps2) (set-subtract scps1 scps2))

; union : Scps Scps -> Scps
(define (union scps1 scps2) (set-union scps1 scps2))


(define (add . args)        (error "must not be used"))
(define (add-stl . args)    (error "must not be used"))
(define (flip . args)       (error "must not be used"))
(define (flip-stl . args)   (error "must not be used"))
(define (at-phase . args)   (error "must not be used"))
(define (prune . args)      (error "must not be used"))
(define (update-ctx . args) (error "must not be used"))
