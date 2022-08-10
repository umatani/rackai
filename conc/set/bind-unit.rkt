#lang racket/unit
(require
 (only-in racket identity)
 (for-syntax racket)
 racket/match
 "../../set.rkt"
 "../../nondet.rkt"
 (only-in "../../term.rkt" use-terms)

 (only-in "../../signatures.rkt"
          syntax^ mstore^ bind^)
 (only-in "../../terms.rkt"
          Sym% Stx%
          [#%term-forms tm:#%term-forms])
 (only-in "../../config.rkt" config^ [#%term-forms cfg:#%term-forms]))

(import (only config^
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
  ;(printf "SET RESOLVE\n")
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbss (filter set? (set->list (results (lookup-Σ Σ0 nam))))]
           ;[_ (printf "sbss: ~a\n" sbss)]
           [scpsss
            (map (λ (sbs) (set-map sbs (λ (sb) (StoBind-scps sb))))
                 sbss)]
           ;[_ (printf "scpsss: ~a\n" scpsss)]
           [scps_biggests (map (λ (scpss)
                                 (biggest-subset
                                  (if ph (at-phase ctx ph) ctx)
                                  scpss))
                               scpsss)]
           ;[_ (printf "scps_biggests: ~a\n" scps_biggests)]
           [nam_biggests
            (filter identity
                    (for*/list ([sbs (in-list sbss)]
                                [scps_biggest (in-list scps_biggests)])
                      (binding-lookup sbs scps_biggest)))])
      ;(printf "nam_biggests: ~a\n" nam_biggests)
      (lift (if (null? nam_biggests)
                (set nam)
                (list->set nam_biggests))))))

; id=? : Ph Id Nam ξ Σ -> Boolean
;   ξ is non-#f only in full
(define (id=? #:phase [ph #f] id nam #:ξ [ξ #f] Σ)
  (subset? (set nam) (results (resolve #:phase ph id Σ))))
