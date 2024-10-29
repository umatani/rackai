#lang racket/unit
(require
 (only-in racket/match       match-let match-define)
 (only-in "../../set.rkt"    set set? ∅ ∅? set-add for/set)
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
                                #:when (set? sbs)     ;; (Setof StoBind)
                                (pure (set-add sbs (StoBind (at-phase ctx ph)
                                                            nam₀))))))
                         (set ∅)))))

;; resolve : Ph Id Σ → (SetM Nam)
(define (resolve ph id Σ)
  (match-define (Stx (Sym nam) ctx) id)
  (do sbs          <- (lookup-Σ Σ nam)
      #:when (set? sbs)                  ;; (Setof StoBind)
      scpss        := (for/set ([sb sbs]) (StoBind-scps sb))
      scps_biggest := (biggest-subset (at-phase ctx ph) scpss)
      nam_biggest  := (binding-lookup sbs scps_biggest)
      (pure (or nam_biggest nam))))
