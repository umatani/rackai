#lang racket/unit
(require
 (only-in racket/match       match-let match-define)
 (only-in "../../set.rkt"    set ∅ ∅? set? set-add for/set)
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
                                #:when (set? sbs)     ;; (Setof StoBind)
                                (pure (set-add sbs (StoBind ctx nam₀))))))
                         (set ∅)))))

;; resolve : Id Σ → (SetM Nam)
(define (resolve id Σ)
  (match-define (Stx (Sym nam) ctx) id)
  (do sbs          <- (lookup-Σ Σ nam)
      #:when (set? sbs)                  ;; (Setof StoBind)
      scpss        := (for/set ([sb sbs]) (StoBind-scps sb))
      scps_biggest := (biggest-subset ctx scpss)
      nam_biggest  := (binding-lookup sbs scps_biggest)
      (pure (or nam_biggest nam))))
#;
(define (resolve id Σ)
  (match-define (Stx (Sym nam) ctx) id)
  (define nams
    (do sbs          <- (lookup-Σ Σ nam)
        #:when (set? sbs)                  ;; (Setof StoBind)
        scpss        := (for/set ([sb sbs]) (StoBind-scps sb))
        scps_biggest := (biggest-subset ctx scpss)
        nam_biggest  := (binding-lookup sbs scps_biggest)
        #:when nam_biggest
        (pure nam_biggest)))
  (if (∅? (results nams))
    (pure nam)
    nams))
