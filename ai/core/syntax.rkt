#lang racket
(require "../../interp/core/struct.rkt"
         (only-in "../../interp/core/syntax.rkt" biggest-subset binding-lookup))
(provide (all-defined-out))

;; Add a binding using the name and scopes of an identifier, mapping
;; them in the store to a given name
;(: bind : Σ Id Nam -> Σ)
(define (bind Σ0 id nam)
  (match-let ([(Σ size tbl) Σ0]
              [(GenStx (Sym nam_1) ctx_1) id])
    (Σ size (hash-update tbl nam_1
                          (λ (sbs) (set-add sbs (StoBind ctx_1 nam)))
                          (λ () (set))))))

;(: lookup-Σ : Σ Nam -> (U (Setof StoBind) Val ξ))
(define (lookup-Σ Σ0 nam)
  (hash-ref (Σ-tbl Σ0) nam (λ () (set))))

;(: resolve : Id Σ -> Nam)
(define (resolve id Σ0)
  (match-let ([(GenStx (Sym nam) ctx) id])
    (let* ([sbs (lookup-Σ Σ0 nam)]
           [scpss (map (λ (sb) (StoBind-scps sb)) (set->list sbs))]
           [scps_biggest (biggest-subset ctx scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))
