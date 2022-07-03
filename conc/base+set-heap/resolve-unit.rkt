#lang racket/unit
(require
 (only-in racket identity)
 racket/match
 "../../set.rkt"
 "../../nondet.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          syntax^ resolve^ mstore^)
 (only-in "../../terms.rkt" terms^ #%term-forms))

(import (only terms^
              Sym% Stx% StoBind%)
        (only syntax^
              binding-lookup biggest-subset at-phase)
        (only mstore^
              lookup-Σ))
(export resolve^)

(use-terms Sym Stx StoBind)

; resolve : Ph Id Σ -> (SetM Nam)
;   ph is #f means called from core
(define (resolve #:phase [ph #f] id Σ0)
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbss (filter set? (set->list (car (do (lookup-Σ Σ0 nam)))))]
           [scpsss
            (map (λ (sbs) (set-map sbs (λ (sb) (StoBind-scps sb))))
                 sbss)]
           [scps_biggests (map (λ (scpss)
                                 (biggest-subset
                                  (if ph (at-phase ctx ph) ctx)
                                  scpss))
                               scpsss)]
           [nam_biggests
            (filter identity
                    (for*/list ([sbs (in-list sbss)]
                                [scps_biggest (in-list scps_biggests)])
                      (binding-lookup sbs scps_biggest)))])
      (lift (if (null? nam_biggests)
                (set nam)
                (list->set nam_biggests))))))

; id=? : Ph Id Nam ξ Σ -> Boolean
;   ph is #f means called from core
;   ξ  is always #f (used only from full)
(define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
  (subset? (set nam) (car (do (resolve #:phase ph id Σ)))))
