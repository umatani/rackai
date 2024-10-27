#lang racket/unit
(require
 (only-in racket/match       match-let match-define)
 (only-in "../../set.rkt"    set set? ∅ ∅? set-add set-map)
 (only-in "../../nondet.rkt" do := <- pure lift results)
 "../../signatures.rkt"
 "../../base/phases/terms.rkt"
 (only-in "../../misc.rkt"   biggest-subset binding-lookup))

(import (only syntax^    at-phase)
        (only mstore^    lookup-Σ))
(export bind^)

;; bind : Ph Σ Id Nam → Σ
(define (bind ph Σ₀ id nam₀)
  (match-let ([(Σ size tbl) Σ₀]
              [(Stx (Sym nam) ctx) id])
    (Σ size (hash-update tbl nam
                         (λ (sbss)
                           (results
                            (do sbs <- (lift sbss)
                                #:when (set? sbs)
                                (pure (set-add sbs (StoBind (at-phase ctx ph)
                                                            nam₀))))))
                         (set ∅)))))

;; resolve : Ph Id Σ → (SetM Nam)
(define (resolve ph id Σ₀)
  (match-define (Stx (Sym nam) ctx) id)
  (define nams (do sbs          <- (lookup-Σ Σ₀ nam)
                   #:when (set? sbs)
                   scpss        := (set-map (λ (sb) (StoBind-scps sb)) sbs)
                   scps_biggest := (biggest-subset (at-phase ctx ph) scpss)
                   nam_biggest  := (binding-lookup sbs scps_biggest)
                   #:when nam_biggest
                   (pure nam_biggest)))
  (if (∅? (results nams))
    (pure nam)
    nams))
