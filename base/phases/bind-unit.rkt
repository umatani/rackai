#lang racket/unit
(require
 (only-in racket/match     match match-let)
 (only-in "../../set.rkt"  ∅ set-add for/set)
 "../../signatures.rkt"
 "../../terms.rkt"
 (only-in "../../misc.rkt" biggest-subset binding-lookup))

(import (only   menv^    extend-ξ)
        (only mstore^    lookup-Σ alloc-name)
        (only syntax^    at-phase add))
(export bind^)


;; bind : Ph Σ Id Nam → Σ
;;   Like one-phase `bind', but extracts scopes at a given phase of
;;   the identifier
(define (bind ph Σ₀ id nam₀)
  (match-let ([(Σ size tbl) Σ₀]
              [(Stx (Sym nam) ctx) id])
    (Σ size (hash-update tbl nam
                         (λ (sbs) (set-add sbs (StoBind (at-phase ctx ph) nam₀)))
                         ∅))))

;; resolve : Ph Id Σ → Nam
(define (resolve ph id Σ₀)
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbs          (lookup-Σ Σ₀ nam)]
           [scpss        (for/set ([sb sbs]) (StoBind-scps sb))]
           [scps_biggest (biggest-subset (at-phase ctx ph) scpss)]
           [nam_biggest  (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))
