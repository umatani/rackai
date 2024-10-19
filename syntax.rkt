#lang racket/base
(require
 (only-in racket/match match match* match-λ)
 (only-in "set.rkt"    ∈ set-add set-remove set-subtract)
 "terms.rkt")
(provide (all-defined-out))

;; ----------------------------------------
;; Syntax-object operations:

;; syntax->datum (especially useful for displaying κ)
(define (stx->datum stx)
  (cond
    [(Hole? stx) (Sym '□)]
    [(Stxξ? stx) (stx->datum (Stxξ-stx stx))]
    [(Stx? stx) (let ([e (Stx-e stx)])
                  (cond
                    [(prim? e) (Sym e)]
                    [(Stx? e)  (stx->datum e)]
                    [(Atom? e) e]
                    [(Null? e) (Null)]
                    [(Pair? e) (Pair (stx->datum (Pair-a e))
                                     (stl->datum (Pair-d e)))]
                    [else e]))]
    [else stx]))

(define (stl->datum stl)
  (cond
    [(Hole? stl) (Sym '□)]
    [(Null? stl) (Null)]
    [(Stx? stl)  (stx->datum stl)]
    [(Pair? stl) (Pair (stx->datum (Pair-a stl))
                       (stl->datum (Pair-d stl)))]
    [else stl]))


;; snoc : ProperStl Stx -> ProperStl
(define (snoc stl stx)
  (cond
    [(Null? stl) (Pair stx (Null))]
    [(Pair? stl) (Pair (Pair-a stl) (snoc (Pair-d stl) stx))]
    [else (error "no such case")]))
  
;; zip : ProperStl ProperStl Ctx → ProperStl
(define (zip stl stl′ ctx)
  (match* (stl stl′)
    [((Null) (Null))
     (Null)]
    [((Pair stxₗ stlₗ) (Pair stxᵣ stlᵣ))
     (Pair (Stx (Pair stxₗ (Pair stxᵣ (Null))) ctx)
           (zip stlₗ stlᵣ ctx))]))

;; unzip : ProperStl → (Values ProperStl ProperStl)
(define (unzip stl)
  (match stl
    [(Null)
     (values (Null) (Null))]
    [(Pair (Stx (Pair stxₗ (Pair stxᵣ (Null))) _) stl)
     (let-values ([(stlₗ stlᵣ) (unzip stl)])
       (values (Pair stxₗ stlₗ) (Pair stxᵣ stlᵣ)))]))

;; map-ctx : Stx (→ Ctx Ctx) → Stx
(define (map-ctx stx f)
  (define mc-stx
    (match-λ
     [(Stx (Null) ctx)
      (Stx (Null) (f ctx))]
     [(Stx (Pair stx stl) ctx)
      (Stx (Pair (mc-stx stx) (mc-stl stl)) (f ctx))]
     [(Stx (? Atom? atom) ctx)
      (Stx atom (f ctx))]
     [(Stx (? prim? prim) ctx)
      (Stx prim (f ctx))]))
  (define mc-stl
    (match-λ
     [(? Stx? stx)   (mc-stx stx)]
     [(Null)         (Null)]
     [(Pair stx stl) (Pair (mc-stx stx) (mc-stl stl))]))
  (mc-stx stx))


;; in-hole-stl : (Stx Stx → Stx) Stl Stx → Stl
(define (in-hole-stl in-hole stl x)
  (match stl
    [(? Stx? stx)   (in-hole stx x)]
    [(Pair stx stl) (Pair (in-hole stx x) (in-hole-stl in-hole stl x))]
    [(Hole)         x]
    [_              stl]))

;; strip : Stl → Val
;;   Recursively strips lexical context from a syntax object
(define (strip stl)
  (match stl
    [(Stx (Pair stx stl) _)
     (Pair (strip stx) (strip stl))]
    [(Stx a _)
     a]
    [(Null)
     (Null)]
    [(Pair stx stl)
     (Pair (strip stx) (strip stl))]))

;; ⊕ : Scp Scps → Scps
;;   Adds or cancels a scope
(define (⊕ scp scps)
  (if (∈ scp scps)
    (set-remove scps scp)
    (set-add scps scp)))
