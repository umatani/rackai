#lang racket
(require "../set.rkt" "../dprint.rkt"
         (only-in "../core/syntax.rkt"
                  in-hole-stl addremove subtract
                  lookup-Σ biggest-subset binding-lookup)
         "struct.rkt")
(provide (all-defined-out))

(define (empty-ctx) (make-immutable-hash))

;(: in-hole : Stx Stx -> Stx)
(define (in-hole stx v)
  (match stx
    [(Stxξ ph stx ξ scps) (Stxξ ph (in-hole stx v) ξ scps)] ; added
    [(GenStx (? Atom? atom) ctx) (GenStx atom ctx)]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (in-hole stx v) (in-hole-stl in-hole stl v)) ctx)]
    [(Hole) v]
    [_ stx]))

;; ----------------------------------------
;; Syntax-object operations:

;; Similar to one-phase `add`, but must update context
;; at a given phase
;(: add : Ph Stx Scp -> Stx)
(define (add ph stx scp)
  (match stx
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (add ph stx scp) (add-stl ph stl scp))
             (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]))

;(: add-stl : Ph Stl Scp -> Stl)
(define (add-stl ph stl scp)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (add ph stx scp) (add-stl ph stl scp))
             (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (update-ctx ctx ph (set-add (at-phase ctx ph) scp)))]
    [(cons stx stl) (cons (add ph stx scp) (add-stl ph stl scp))]))


;; Similar to one-phase `flip`, but must update context
;; at a given phase
;(: flip : Ph Stx Scp -> Stx)
(define (flip ph stx scp)
  (match stx
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (flip ph stx scp) (flip-stl ph stl scp))
             (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]))

;(: flip-stl : Ph Stl Scp -> Stl)
(define (flip-stl ph stl scp)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (flip ph stx scp) (flip-stl ph stl scp))
             (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (update-ctx ctx ph (addremove scp (at-phase ctx ph))))]
    [(cons stx stl) (cons (flip ph stx scp) (flip-stl ph stl scp))]))

;; Recursively removes a set of scopes from a syntax object
;; at a given phase
;(: prune : Ph Stx Scps -> Stx)
(define (prune ph stx scps_p)
  (match stx
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (prune ph stx scps_p) (prune-stl ph stl scps_p))
             (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]))

;(: prune-stl : Ph Stl Scps -> Stl)
(define (prune-stl ph stl scps_p)
  (match stl
    ['() '()]
    [(GenStx (cons stx stl) ctx)
     (GenStx (cons (prune ph stx scps_p) (prune-stl ph stl scps_p))
             (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
    [(GenStx (? Atom? atom) ctx)
     (GenStx atom (update-ctx ctx ph (subtract (at-phase ctx ph) scps_p)))]
    [(cons stx stl) (cons (prune ph stx scps_p) (prune-stl ph stl scps_p))]))

;; Updates the mapping of a `ctx` at a particular phase
;(: update-ctx : Ctx Ph Scps -> Ctx)
(define (update-ctx ctx ph scps)
  (dict-set ctx ph scps))

;; Like one-phase `bind`, but extracts scopes at a given phase of
;; the identifier
;(: bind : Ph Σ Id Nam -> Σ)
(define (bind ph Σ0 id nam)
  (dprint 'phases 'bind "")
  (match-let ([(Σ size tbl) Σ0]
              [(GenStx (Sym nam_1) ctx_1) id])
    (Σ size (hash-update
              tbl nam_1
              (λ (sbs) (set-add sbs (StoBind (at-phase ctx_1 ph) nam)))
              (λ () (set))))))

;(: at-phase : Ctx Ph -> Scps)
(define (at-phase ctx ph)
  (dict-ref ctx ph (λ () (set))))

;; Like the one-phase `resolve`, but at a particular phase
;(: resolve : Ph Id Σ -> Nam)
(define (resolve ph id Σ0)
  (match-let ([(GenStx (Sym nam) ctx) id])
    (let* ([sbs (lookup-Σ Σ0 nam)]
           [scpss (map (λ (sb) (StoBind-scps sb)) (set->list sbs))]
           [scps_biggest (biggest-subset (at-phase ctx ph) scpss)]
           [nam_biggest (binding-lookup sbs scps_biggest)])
      (or nam_biggest nam))))

;(: id=? : ph Id Nam Σ -> Boolean)
(define (id=? ph id nam Σ) (eq? (resolve ph id Σ) nam))
