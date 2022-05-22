#lang racket
(require "../../interp/phases/struct.rkt"
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
    (Σ size (hash-update
              tbl nam_1
              (λ (sbs) (set-add sbs (StoBind (at-phase ctx_1 ph) nam)))
              (λ () (set))))))

;; Like the one-phase `resolve`, but at a particular phase
;(: resolve : Ph Id Σ -> Nam)
(define (resolve ph id Σ0)
  (match-let ([(GenStx (Sym nam) ctx) id])
    (let* ([sbs (lookup-Σ Σ0 nam)]
           [scpss (map (λ (sb) (StoBind-scps sb)) (set->list sbs))]
           [scps_biggest (biggest-subset (at-phase ctx ph) scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))
