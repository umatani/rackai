#lang racket/unit
(require
 (only-in racket/match     match-define)
 (only-in "../../set.rkt"  set-add for/set)
 "../../signatures.rkt"
 "../../terms.rkt"
 (only-in "../../misc.rkt" biggest-subset binding-lookup))

(import (only mstore^    lookup-Σ update-Σ)
        (only syntax^    at-phase))
(export bind^)


;; bind : Ph Σ Id Nam → Σ
;;   Like one-phase `bind', but extracts scopes at a given phase of
;;   the identifier
(define (bind ph Σ id nam₀)
  (match-define (Stx (Sym nam) ctx) id)
  (update-Σ Σ nam (set-add (lookup-Σ Σ nam) (StoBind (at-phase ctx ph) nam₀))))

;; resolve : Ph Id Σ → Nam
(define (resolve ph id Σ₀)
  (match-define (Stx (Sym nam) ctx) id)
  (let* ([sbs          (lookup-Σ Σ₀ nam)]
         [scpss        (for/set ([sb sbs]) (StoBind-scps sb))]
         [scps_biggest (biggest-subset (at-phase ctx ph) scpss)]
         [nam_biggest  (binding-lookup sbs scps_biggest)])
    (or nam_biggest nam)))
