#lang racket/unit
(require
 (only-in racket/match match-let)
 (only-in "../set.rkt" set set-add set->list)
 "../signatures.rkt"
 "../terms.rkt"
 (only-in "../misc.rkt" biggest-subset binding-lookup))

(import (only syntax^    at-phase)
        (only mstore^    lookup-Σ))
(export bind^)


;; bind : Ph? Σ Id Nam → Σ
;;   When Ph is not #f, like one-phase `bind', but extracts scopes at
;;   a given phase of the identifier
(define (bind #:phase [ph #f] Σ₀ id nam)
  (match-let ([(Σ size tbl) Σ₀]
              [(Stx (Sym nam₀) ctx₀) id])
    (Σ size (hash-update tbl nam₀
                         (λ (sbs) (set-add sbs (StoBind
                                                (if ph (at-phase ctx₀ ph) ctx₀)
                                                nam)))
                         (set)))))

;; resolve : Ph Id Σ → Nam
;;   Ph is #f means called from core
(define (resolve #:phase [ph #f] id Σ₀)
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbs (lookup-Σ Σ₀ nam)]
           [scpss (map (λ (sb) (StoBind-scps sb)) (set->list sbs))]
           [scps_biggest (biggest-subset
                          (if ph (at-phase ctx ph) ctx)
                          scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))

;; id=? : Ph Id Nam ξ? Σ → Boolean
;;   ph is #f means called from core
;;   ξ  is usually #f (used only from full)
(define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
  (eq? (resolve #:phase ph id Σ) nam))
