#lang racket/unit
(require
 (only-in racket identity)
 racket/match
 "../../set.rkt"
 "../../nondet.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          syntax^ mstore^ bind^)
 (only-in "../../terms.rkt" terms^ #%term-forms))

(import (only terms^
              Sym% Stx% Σ% StoBind%)
        (only syntax^
              binding-lookup biggest-subset at-phase)
        (only mstore^
              lookup-Σ))
(export bind^)

(use-terms Sym Stx Σ StoBind)

;; ph is #f means called from core

; bind : Ph Σ Id Nam -> Σ
(define (bind #:phase [ph #f] Σ0 id nam)
  (match-let ([(Σ size tbl) Σ0]
              [(Stx (Sym nam_1) ctx_1) id])
    (Σ size
      (hash-update tbl nam_1
                   (λ (sbss)
                     (for/set ([sbs (in-set sbss)]
                               #:when (set? sbs))
                       (set-add sbs (StoBind
                                     (if ph (at-phase ctx_1 ph) ctx_1)
                                     nam))))
                   (λ () (set (set)))))))

; resolve : Ph Id Σ -> (SetM Nam)
(define (resolve #:phase [ph #f] id Σ0)
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbss (filter set? (set->list (results (do (lookup-Σ Σ0 nam)))))]
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
;   ξ is non-#f only in full
(define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
  (subset? (set nam) (results (do (resolve #:phase ph id Σ)))))
