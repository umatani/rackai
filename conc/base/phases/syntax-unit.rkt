#lang racket/unit
(require
 racket/match racket/dict
 "../../../set.rkt"
 
 (only-in "../../../struct-common-sig.rkt" struct-common^)
 (only-in "struct-stxe-sig.rkt"            struct-stxe^)

 (only-in "../../../syntax-sig.rkt"        syntax^)
 (only-in "../../../phase-sig.rkt"         phase^))

(import (only struct-common^
              Stx stx Hole atom?)
        (only struct-stxe^
              Stxξ stx&ξ)
        (prefix core: (only syntax^ ;; from conc/base/core
                            addremove strip subtract union in-hole-stl
                            binding-lookup biggest-subset
                            stl->seq snoc zip unzip)))
(export syntax^ phase^)

;; inherited from conc/base/core
(define stl->seq        core:stl->seq)
(define in-hole-stl     core:in-hole-stl)
(define addremove       core:addremove)
(define strip           core:strip)
(define subtract        core:subtract)
(define union           core:union)
(define binding-lookup  core:binding-lookup)
(define biggest-subset  core:biggest-subset)
(define snoc            core:snoc)
(define zip             core:zip)
(define unzip           core:unzip)


(define (empty-ctx) (make-immutable-hash))

;(: in-hole : Stx Stx -> Stx)
(define (in-hole stx0 v)
  (match stx0
    [(Stxξ ph stx1 ξ scps) (stx&ξ ph (in-hole stx1 v) ξ scps)] ; added
    [(Stx (? atom? atom) ctx) (stx atom ctx)]
    [(Stx (cons stx1 stl) ctx)
     (stx (cons (in-hole stx1 v) (core:in-hole-stl in-hole stl v)) ctx)]
    [(Hole) v]
    [_ stx0]))

;; ----------------------------------------
;; Syntax-object operations:

; at-phase : Ctx Ph -> Scps
(define (at-phase ctx ph)
  (dict-ref ctx ph (λ () (set))))

;; Updates the mapping of a `ctx` at a particular phase
; update-ctx : Ctx Ph Scps -> Ctx
(define (update-ctx ctx ph scps)
  (dict-set ctx ph scps))

;; Similar to one-phase `add`, but must update context
;; at a given phase
; add : Ph Stx Scp -> Stx
(define (add ph stx0 scp)
  (match stx0
    [(Stx (cons stx1 stl) ctx)
     (stx (cons (add ph stx1 scp) (add-stl ph stl scp))
             (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
    [(Stx (? atom? atom) ctx)
     (stx atom (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]))

; add-stl : Ph Stl Scp -> Stl
(define (add-stl ph stl scp)
  (match stl
    ['() '()]
    [(Stx (cons stx0 stl) ctx)
     (stx (cons (add ph stx0 scp) (add-stl ph stl scp))
             (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
    [(Stx (? atom? atom) ctx)
     (stx atom (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
    [(cons stx0 stl) (cons (add ph stx0 scp) (add-stl ph stl scp))]))


;; Similar to one-phase `flip`, but must update context
;; at a given phase
; flip : Ph Stx Scp -> Stx
(define (flip ph stx0 scp)
  (match stx0
    [(Stx (cons stx1 stl) ctx)
     (stx (cons (flip ph stx1 scp) (flip-stl ph stl scp))
          (update-ctx ctx ph (core:addremove scp (at-phase ctx ph))))]
    [(Stx (? atom? atom) ctx)
     (stx atom (update-ctx ctx ph (core:addremove scp (at-phase ctx ph))))]))

; flip-stl : Ph Stl Scp -> Stl
(define (flip-stl ph stl scp)
  (match stl
    ['() '()]
    [(Stx (cons stx0 stl) ctx)
     (stx (cons (flip ph stx0 scp) (flip-stl ph stl scp))
          (update-ctx ctx ph (core:addremove scp (at-phase ctx ph))))]
    [(Stx (? atom? atom) ctx)
     (stx atom (update-ctx ctx ph (core:addremove scp (at-phase ctx ph))))]
    [(cons stx0 stl) (cons (flip ph stx0 scp) (flip-stl ph stl scp))]))

;; Recursively removes a set of scopes from a syntax object
;; at a given phase
; prune : Ph Stx Scps -> Stx
(define (prune ph stx0 scps_p)
  (match stx0
    [(Stx (cons stx1 stl) ctx)
     (stx (cons (prune ph stx1 scps_p) (prune-stl ph stl scps_p))
          (update-ctx ctx ph (core:subtract (at-phase ctx ph) scps_p)))]
    [(Stx (? atom? atom) ctx)
     (stx atom (update-ctx ctx ph (core:subtract (at-phase ctx ph) scps_p)))]))

; prune-stl : Ph Stl Scps -> Stl
(define (prune-stl ph stl scps_p)
  (match stl
    ['() '()]
    [(Stx (cons stx0 stl) ctx)
     (stx (cons (prune ph stx0 scps_p) (prune-stl ph stl scps_p))
          (update-ctx ctx ph (core:subtract (at-phase ctx ph) scps_p)))]
    [(Stx (? atom? atom) ctx)
     (stx atom (update-ctx ctx ph (core:subtract (at-phase ctx ph) scps_p)))]
    [(cons stx0 stl) (cons (prune ph stx0 scps_p) (prune-stl ph stl scps_p))]))
