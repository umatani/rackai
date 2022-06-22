#lang racket/unit
(require (except-in racket set do)
         "../../../set.rkt" "../../../nondet.rkt"

         (only-in "../../../struct-common-sig.rkt" struct-common^)
         (only-in "../../../syntax-sig.rkt" syntax^)
         (only-in "../../../phase-sig.rkt" phase^)
         (only-in "../../../mstore-sig.rkt" mstore^))

(import ; from conc/base
        (only struct-common^
              Σ mk-Σ Stx Sym stobind StoBind-scps)
        (only syntax^ biggest-subset binding-lookup)
        (only phase^ at-phase)

        ; set-based version from conc/base+set-heap/core
        (prefix core: (only mstore^
                            init-Σ lookup-Σ alloc-name alloc-scope)))
(export mstore^)

(define init-Σ      core:init-Σ)
(define lookup-Σ    core:lookup-Σ)
(define alloc-name  core:alloc-name)
(define alloc-scope core:alloc-scope)


;; Like one-phase `bind`, but extracts scopes at a given phase of
;; the identifier
; bind : Ph Σ Id Nam -> Σ
(define (bind ph Σ0 id nam)
  (match-let ([(Σ size tbl) Σ0]
              [(Stx (Sym nam_1) ctx_1) id])
    (mk-Σ size
           (hash-update tbl nam_1
                        (λ (sbss)
                          (for/set ([sbs (in-set sbss)]
                                    #:when (set? sbs))
                            (set-add sbs (stobind (at-phase ctx_1 ph) nam))))
                        (λ () (set (set)))))))

;; Like the one-phase `resolve`, but at a particular phase
; resolve : Ph Id Σ -> (SetM Nam)
(define (resolve ph id Σ0)
  (match-let ([(Stx (Sym nam) ctx) id])
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

; id=? : Ph Id Nam Σ -> Boolean
(define (id=? ph id nam Σ) (subset? (set nam) (car (do (resolve ph id Σ)))))
