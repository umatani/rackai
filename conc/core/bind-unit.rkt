#lang racket/unit
(require
 (only-in racket/match  match-let)
 (only-in "../../set.rkt"  set set-add set->list)
 "../../signatures.rkt"
 "../../terms.rkt"
 (only-in "../../misc.rkt" biggest-subset binding-lookup))

(import (only mstore^    lookup-Σ))
(export bind^)


;; bind : Σ Id Nam → Σ
(define (bind Σ₀ id nam₀)
  (match-let ([(Σ size tbl) Σ₀]
              [(Stx (Sym nam) ctx) id])
    (Σ size (hash-update tbl nam
                         (λ (sbs) (set-add sbs (StoBind ctx nam₀)))
                         (set)))))

;; resolve : Id Σ → Nam
(define (resolve id Σ₀)
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbs (lookup-Σ Σ₀ nam)]
           [scpss (map (λ (sb) (StoBind-scps sb)) (set->list sbs))]
           [scps_biggest (biggest-subset ctx scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))
