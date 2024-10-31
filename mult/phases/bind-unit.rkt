#lang racket/unit
(require
 (only-in racket/match       match-let match-define)
 (only-in "../../set.rkt"    set set? ∅ ∅? set-add for/set)
 (only-in "../../nondet.rkt" do := <- pure lift results)
 "../../signatures.rkt"
 "../../base/phases/terms.rkt"
 (only-in "../../misc.rkt"   biggest-subset binding-lookup
                             lookup-sbs update-sbs set-of-stobind?))

(import (only syntax^    at-phase)
        (only mstore^    lookup-Σ update-Σ))
(export bind^)

;; bind : Ph Σ Id Nam → Σ
(define (bind ph Σ₀ id nam₀)
  (match-define (Stx (Sym nam) ctx) id)
  (define sbs (results (lookup-Σ Σ₀ nam)))
  (if (and (not (∅? sbs)) (lookup-sbs sbs (at-phase ctx ph)))
    (match-let ([(Σ size tbl) Σ₀])                ;; add to exsisting StoBind
      (Σ size (hash-set tbl nam (update-sbs sbs (at-phase ctx ph) nam₀))))
    (update-Σ Σ₀ nam (StoBind (at-phase ctx ph) (set nam₀)))   ;; new StoBind
    ))

;; resolve : Ph Id Σ → (SetM Nam)
(define (resolve ph id Σ)
  (match-define (Stx (Sym nam) ctx) id)
  (do sbs          := (results (lookup-Σ Σ nam))
      #:when (set-of-stobind? sbs)                ;; (Setof StoBind)
      scpss        := (for/set ([sb sbs]) (StoBind-scps sb))
      scps_biggest := (biggest-subset (at-phase ctx ph) scpss)
      nams_biggest := (binding-lookup sbs scps_biggest)
      (if nams_biggest
        (lift nams_biggest)
        (pure nam))))
