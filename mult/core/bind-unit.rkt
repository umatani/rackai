#lang racket/unit
(require
 (only-in racket/match       match-let match-define)
 (only-in "../../set.rkt"    set ∅ ∅? set? set-add set-map for/set in-set)
 (only-in "../../nondet.rkt" do := <- pure lift results)
 "../../signatures.rkt"
 "../../base/core/terms.rkt"
 (only-in "../../misc.rkt"   biggest-subset binding-lookup))

(import (only mstore^    lookup-Σ))
(export bind^)

;; bind : Σ Id Nam → Σ
(define (bind Σ₀ id nam₀)
  (match-let ([(Σ size tbl) Σ₀]
              [(Stx (Sym nam) ctx) id])
    (Σ size (hash-update tbl nam
                         (λ (sbss)
                           (results
                            (do sbs <- (lift sbss)
                                #:when (set? sbs)
                                (pure (set-add sbs (StoBind ctx nam₀))))))
                         (set ∅)))))

;; resolve : Id Σ → (SetM Nam)
(define (resolve id Σ₀)
  (match-define (Stx (Sym nam) ctx) id)
  (define nams (do sbs          <- (lookup-Σ Σ₀ nam)
                   #:when (set? sbs)
                   scpss        := (set-map (λ (sb) (StoBind-scps sb)) sbs)
                   scps_biggest := (biggest-subset ctx scpss)
                   nam_biggest  := (binding-lookup sbs scps_biggest)
                   #:when nam_biggest
                   (pure nam_biggest)))
  (if (∅? (results nams))
    (pure nam)
    nams))
