#lang racket/unit
(require
 racket/match
 "../../set.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          syntax^ resolve^ mstore^)
 (only-in "../../terms.rkt" terms^ #%term-forms))

(import
 (only terms^
       Sym% Stx% StoBind%)
 (only syntax^
       binding-lookup biggest-subset at-phase)
 (only mstore^
       lookup-Σ))
(export resolve^)

(use-terms Sym Stx StoBind)

;; resolve : Ph Id Σ -> Nam
;;   Ph is #f means called from core
(define (resolve #:phase [ph #f] id Σ0) 
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbs (lookup-Σ Σ0 nam)]
           [scpss (map (λ (sb) (StoBind-scps sb)) (set->list sbs))]
           [scps_biggest (biggest-subset
                          (if ph (at-phase ctx ph) ctx)
                          scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))

;; id=? : Ph Id Nam ξ Σ -> Boolean
;;   ph is #f means called from core
;;   ξ  is always #f (used only from full)
(define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
  (eq? (resolve #:phase ph id Σ) nam))
