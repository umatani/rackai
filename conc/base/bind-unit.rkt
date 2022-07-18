#lang racket/unit
(require
 racket/match
 (for-syntax racket)
 "../../set.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          config^ syntax^ mstore^ bind^)
 (only-in "../../terms.rkt"
          Sym% Stx% [#%term-forms tm:#%term-forms])
 (only-in "../../config.rkt" [#%term-forms cfg:#%term-forms]))

(import
 (only config^
       Σ% StoBind%)
 (only syntax^
       binding-lookup biggest-subset at-phase)
 (only mstore^
       lookup-Σ))
(export bind^)

(define-syntax #%term-forms
  (append (syntax-local-value #'tm:#%term-forms)
          (syntax-local-value #'cfg:#%term-forms)))
(use-terms Sym Stx Σ StoBind)


;; Like one-phase `bind`, but extracts scopes at a given phase of
;; the identifier
;(: bind : Ph Σ Id Nam -> Σ)
(define (bind #:phase [ph #f] Σ0 id nam)
  (match-let ([(Σ size tbl) Σ0]
              [(Stx (Sym nam_1) ctx_1) id])
    (Σ size (hash-update
              tbl nam_1
              (λ (sbs) (set-add sbs (StoBind
                                      (if ph (at-phase ctx_1 ph) ctx_1)
                                      nam)))
              (λ () (set))))))

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
