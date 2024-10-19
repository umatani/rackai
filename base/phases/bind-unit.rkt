#lang racket/unit
(require
 (only-in racket/match     match-let)
 (only-in "../../set.rkt"  ∅ set-add set→list)
 "../../signatures.rkt"
 "../../terms.rkt"
 (only-in "../../misc.rkt" biggest-subset binding-lookup))

(import (only syntax^    at-phase)
        (only mstore^    lookup-Σ))
(export bind^)


;; bind : Ph Σ Id Nam → Σ
;;   Like one-phase `bind', but extracts scopes at a given phase of
;;   the identifier
(define (bind ph Σ₀ id nam)
  (match-let ([(Σ size tbl) Σ₀]
              [(Stx (Sym nam₀) ctx₀) id])
    (Σ size (hash-update tbl nam₀
                         (λ (sbs)
                           (set-add sbs (StoBind (at-phase ctx₀ ph) nam)))
                         ∅))))

;; resolve : Ph Id Σ → Nam
(define (resolve ph id Σ₀)
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbs (lookup-Σ Σ₀ nam)]
           [scpss (map (λ (sb) (StoBind-scps sb)) (set→list sbs))]
           [scps_biggest (biggest-subset (at-phase ctx ph) scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))
