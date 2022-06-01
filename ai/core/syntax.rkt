#lang racket
(require "../../interp/set.rkt"
         "../../interp/nondet.rkt"
         "../../interp/core/struct.rkt"
         (only-in "../../interp/core/syntax.rkt" biggest-subset binding-lookup))
(provide (all-defined-out))

;; Set-based Σ

;; Add a binding using the name and scopes of an identifier, mapping
;; them in the store to a given name
;(: bind : Σ Id Nam -> Σ)
(define (bind Σ0 id nam)
  (match-let ([(Σ size tbl) Σ0]
              [(GenStx (Sym nam_1) ctx_1) id])
    (Σ size
      (hash-update tbl nam_1
                   (λ (sbss)
                     (for/set ([sbs (in-set sbss)]
                               #:when (set? sbs))
                       (set-add sbs (StoBind ctx_1 nam))))
                   (λ () (set (set)))))))

;(: lookup-Σ : Σ Nam -> (SetM (U (Setof StoBind) Val ξ))
(define (lookup-Σ Σ0 nam)
  (lift (hash-ref (Σ-tbl Σ0) nam (λ () (set)))))


;(: resolve : Id Σ -> (SetM Nam)
(define (resolve id Σ0)
  (match-let ([(GenStx (Sym nam) ctx) id])
    (let* ([sbss (filter set? (set->list
                               (car (do (lookup-Σ Σ0 nam)))))]
           [scpsss
            (map (λ (sbs) (set-map sbs (λ (sb) (StoBind-scps sb))))
                 sbss)]
           [scps_biggests (map (λ (scpss) (biggest-subset ctx scpss))
                               scpsss)]
           [nam_biggests
            (filter identity
                    (for*/list ([sbs (in-list sbss)]
                                [scps_biggest (in-list scps_biggests)])
                      (binding-lookup sbs scps_biggest)))])
      (lift (if (null? nam_biggests)
                (set nam)
                (list->set nam_biggests))))))

;(: id=? : Id Nam Σ -> Boolean)
(define (id=? id nam Σ) (subset? (set nam) (car (do (resolve id Σ)))))
