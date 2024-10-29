#lang racket/unit
(require
 (only-in racket/match     match match-let)
 (only-in "../../set.rkt"  set ∅ set-add for/set)
 "../../signatures.rkt"
 "../../terms.rkt"
 (only-in "../../misc.rkt" biggest-subset binding-lookup))

(import (only   menv^    extend-ξ)
        (only mstore^    lookup-Σ alloc-name)
        (only syntax^    add))
(export bind^)


;; bind : Σ Id Nam → Σ
(define (bind Σ₀ id nam₀)
  (match-let ([(Σ size tbl) Σ₀]
              [(Stx (Sym nam) ctx) id])
    (Σ size (hash-update tbl nam
                         (λ (sbs) (set-add sbs (StoBind ctx nam₀)))
                         ∅))))

;; resolve : Id Σ → Nam
(define (resolve id Σ)
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbs          (lookup-Σ Σ nam)]
           [scpss        (for/set ([sb sbs]) (StoBind-scps sb))]
           [scps_biggest (biggest-subset ctx scpss)]
           [nam_biggest  (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))
