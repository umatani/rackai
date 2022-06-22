#lang racket/unit
(require
 racket/match
 "../../../set.rkt"
 (only-in "../../../dprint.rkt" dprint)

 (only-in "../../../struct-common-sig.rkt" struct-common^)

 (only-in "../../../syntax-sig.rkt"        syntax^)
 (only-in "../../../phase-sig.rkt"         phase^)
 (only-in "../../../mstore-sig.rkt"        mstore^))

(import (only struct-common^
              Stx Sym Σ mk-Σ stobind StoBind-scps)
        (only syntax^
              binding-lookup biggest-subset)
        (only phase^ at-phase)
        (prefix core: (only mstore^
                            init-Σ lookup-Σ alloc-name alloc-scope)))
(export mstore^)

;; inherited from conc/base/core
(define init-Σ      core:init-Σ)
(define lookup-Σ    core:lookup-Σ)
(define alloc-name  core:alloc-name)
(define alloc-scope core:alloc-scope)


;; Like one-phase `bind`, but extracts scopes at a given phase of
;; the identifier
;(: bind : Ph Σ Id Nam -> Σ)
(define (bind ph Σ0 id nam)
  (dprint 'phases 'bind "")
  (match-let ([(Σ size tbl) Σ0]
              [(Stx (Sym nam_1) ctx_1) id])
    (mk-Σ size (hash-update
                 tbl nam_1
                 (λ (sbs) (set-add sbs (stobind (at-phase ctx_1 ph) nam)))
                 (λ () (set))))))

;; Like the one-phase `resolve`, but at a particular phase
;(: resolve : Ph Id Σ -> Nam)
(define (resolve ph id Σ0)
  (match-let ([(Stx (Sym nam) ctx) id])
    (let* ([sbs (core:lookup-Σ Σ0 nam)]
           [scpss (map (λ (sb) (StoBind-scps sb)) (set->list sbs))]
           [scps_biggest (biggest-subset (at-phase ctx ph) scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))

;(: id=? : ph Id Nam Σ -> Boolean)
(define (id=? ph id nam Σ) (eq? (resolve ph id Σ) nam))
