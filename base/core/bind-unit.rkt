#lang racket/unit
(require
 (only-in racket/match     match-define)
 (only-in "../../set.rkt"  set-add for/set)
 "../../signatures.rkt"
 "../../terms.rkt"
 (only-in "../../misc.rkt" biggest-subset binding-lookup))

(import (only mstore^    lookup-Σ update-Σ))
(export bind^)


;; bind : Σ Id Nam → Σ
(define (bind Σ id nam₀)
  (match-define (Stx (Sym nam) ctx) id)
  (update-Σ Σ nam (set-add (lookup-Σ Σ nam) (StoBind ctx nam₀))))

;; resolve : Id Σ → Nam
(define (resolve id Σ)
  (match-define (Stx (Sym nam) ctx) id)
  (let* ([sbs          (lookup-Σ Σ nam)]
         [scpss        (for/set ([sb sbs]) (StoBind-scps sb))]
         [scps_biggest (biggest-subset ctx scpss)]
         [nam_biggest  (binding-lookup sbs scps_biggest)])
    (or nam_biggest nam)))
