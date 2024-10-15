#lang racket/unit
(require
 (only-in racket             identity)
 (only-in racket/match       match-let)
 (only-in "../../set.rkt"    set set? subset? set-add in-set for/set
                             set->list list->set set-map)
 (only-in "../../nondet.rkt" results lift)
 "../../signatures.rkt"
 "../../base/core/terms.rkt"
 (only-in "../../misc.rkt"   biggest-subset binding-lookup))

(import (only mstore^    lookup-Σ))
(export bind^)

;; bind : Σ Id Nam → Σ
(define (bind Σ0 id nam)
  (match-let ([(Σ size tbl) Σ0]
              [(Stx (Sym nam_1) ctx_1) id])
    (Σ size
      (hash-update tbl nam_1
                   (λ (sbss)
                     (for/set ([sbs (in-set sbss)]
                               #:when (set? sbs))
                       (set-add sbs (StoBind ctx_1 nam))))
                   (λ () (set (set)))))))

;; resolve : Id Σ → (SetM Nam)
(define (resolve id Σ0)
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbss (filter set? (set->list (results (lookup-Σ Σ0 nam))))]
           [scpsss
            (map (λ (sbs) (set-map sbs (λ (sb) (StoBind-scps sb))))
                 sbss)]
           [scps_biggests (map (λ (scpss)
                                 (biggest-subset ctx scpss))
                               scpsss)]
           [nam_biggests
            (filter identity
                    (for*/list ([sbs (in-list sbss)]
                                [scps_biggest (in-list scps_biggests)])
                      (binding-lookup sbs scps_biggest)))])
      (lift (if (null? nam_biggests)
              (set nam)
              (list->set nam_biggests))))))
