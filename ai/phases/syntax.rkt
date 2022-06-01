#lang racket
(require "../../interp/nondet.rkt"
         "../../interp/phases/struct.rkt"
         (only-in "../../interp/core/syntax.rkt" biggest-subset binding-lookup)
         (only-in "../../interp/phases/syntax.rkt" at-phase)

         ;; Abstract version
         (only-in "../core/syntax.rkt" lookup-Σ))
(provide (all-defined-out))

;; Like one-phase `bind`, but extracts scopes at a given phase of
;; the identifier
;(: bind : Ph Σ Id Nam -> Σ)
(define (bind ph Σ0 id nam)
  (match-let ([(Σ size tbl) Σ0]
              [(GenStx (Sym nam_1) ctx_1) id])
    (Σ size
      (hash-update tbl nam_1
                   (λ (sbss)
                     (for/set ([sbs (in-set sbss)]
                               #:when (set? sbs))
                       (set-add sbs (StoBind (at-phase ctx_1 ph) nam))))
                   (λ () (set (set)))))))

;; Like the one-phase `resolve`, but at a particular phase
;(: resolve : Ph Id Σ -> (SetM Nam))
(define (resolve ph id Σ0)
  (match-let ([(GenStx (Sym nam) ctx) id])
    (let* ([sbss (filter set? (set->list (car (do (lookup-Σ Σ0 nam)))))]
           [scpsss
            (map (λ (sbs) (set-map sbs (λ (sb) (StoBind-scps sb))))
                 sbss)]
           [scps_biggests (map (λ (scpss) (biggest-subset
                                            (at-phase ctx ph) scpss))
                               scpsss)]
           [nam_biggests
            (filter identity
                    (for*/list ([sbs (in-list sbss)]
                                [scps_biggest (in-list scps_biggests)])
                      (binding-lookup sbs scps_biggest)))])
      (lift (if (null? nam_biggests)
                (set nam)
                (list->set nam_biggests))))))

;(: id=? : Ph Id Nam Σ -> Boolean)
(define (id=? ph id nam Σ) (subset? (set nam) (car (do (resolve ph id Σ)))))
