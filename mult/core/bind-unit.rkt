#lang racket/unit
(require
 (only-in racket/match       match-let match-define)
 (only-in "../../set.rkt"    set ∅ ∅? set? set=? set-size set-add
          set→list for/set in-set)
 (only-in "../../nondet.rkt" do := <- pure lift results)
 (only-in "../../misc.rkt"   biggest-subset binding-lookup
                             lookup-sbs update-sbs set-of-stobind?)
 "../../signatures.rkt"
 "../../base/core/terms.rkt")

(import (only mstore^    lookup-Σ update-Σ))
(export bind^)


;; bind : Σ Id Nam → Σ
(define (bind Σ₀ id nam₀)
  (match-define (Stx (Sym nam) ctx) id)
  (define sbs (results (lookup-Σ Σ₀ nam)))
  (if (and (not (∅? sbs)) (lookup-sbs sbs ctx))
    (match-let ([(Σ size tbl) Σ₀])               ;; add to exsisting StoBind
      (Σ size (hash-set tbl nam (update-sbs sbs ctx nam₀))))
    (update-Σ Σ₀ nam (StoBind ctx (set nam₀)))   ;; new StoBind
    ))

;; resolve : Id Σ → (SetM Nam)
(define (resolve id Σ)
  (match-define (Stx (Sym nam) ctx) id)
  (do sbs          := (results (lookup-Σ Σ nam))
      #:when (set-of-stobind? sbs)               ;; (Setof StoBind)
      scpss        := (for/set ([sb sbs]) (StoBind-scps sb))
      scps_biggest := (biggest-subset ctx scpss)
      nams_biggest := (binding-lookup sbs scps_biggest)
      (if nams_biggest
        (lift nams_biggest)
        (pure nam))))
