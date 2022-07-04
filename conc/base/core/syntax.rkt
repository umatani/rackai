#lang racket
(require
 "../../../set.rkt"
 (only-in "../../../term.rkt" use-terms)
 
 (only-in "../../../signatures.rkt" terms-extra^ syntax^)
 (only-in "terms.rkt" terms^ #%term-forms)

 (only-in "../units.rkt" [syntax@ super:syntax@]))
(provide syntax@)

(define-unit syntax/super@
  (import (prefix super: syntax^)
          (only terms^       Stx% Hole%)
          (only terms-extra^ atom?))
  (export syntax^)

  (use-terms Stx Hole)

  (define stl->seq       super:stl->seq)
  (define zip            super:zip)
  (define unzip          super:unzip)
  (define snoc           super:snoc)
  (define in-hole-stl    super:in-hole-stl)
  (define addremove      super:addremove)
  (define strip          super:strip)
  (define subtract       super:subtract)
  (define union          super:union)
  (define binding-lookup super:binding-lookup)
  (define biggest-subset super:biggest-subset)

  ;; not used in core
  (define at-phase       super:at-phase)
  (define prune          super:prune)
  (define update-ctx     super:update-ctx)

  ;; ----------------------------------------
  ;; stx utils

  (define (empty-ctx) (set))

  ; in-hole : Stx Stx -> Stl
  (define (in-hole stx v)
    (match stx
      [(Stx (? atom? atom) ctx) (Stx atom ctx)]
      [(Stx (cons stx stl) ctx)
       (Stx (cons (in-hole stx v) (in-hole-stl in-hole stl v)) ctx)]
      [(Hole) v]
      [_ stx]))

  ;; ----------------------------------------
  ;; Syntax-object operations:

  ;; Simply pushes scopes down through a syntax object
  ; add : Stx Scp -> Stx
  (define (add stx scp)
    (match stx
      [(Stx (cons stx stl) ctx)
       (Stx (cons (add stx scp) (add-stl stl scp))
            (set-add ctx scp))]
      [(Stx (? atom? atom) ctx)
       (Stx atom (set-add ctx scp))]))

  ; add-stl : Stl Scp -> Stl
  (define (add-stl stl scp)
    (match stl
      ['() '()]
      [(Stx (cons stx stl) ctx)
       (Stx (cons (add stx scp) (add-stl stl scp))
            (set-add ctx scp))]
      [(Stx (? atom? atom) ctx) (Stx atom (set-add ctx scp))]
      [(cons stx stl) (cons (add stx scp) (add-stl stl scp))]))

  ;; Pushes flipping a scope down through a syntax object
  ; flip : Stx Scp -> Stx
  (define (flip stx scp)
    (match stx
      [(Stx (cons stx stl) ctx)
       (Stx (cons (flip stx scp) (flip-stl stl scp))
            (addremove scp ctx))]
      [(Stx (? atom? atom) ctx)
       (Stx atom (addremove scp ctx))]))

  ; flip-stl : Stl Scp -> Stl
  (define (flip-stl stl scp)
    (match stl
      ['() '()]
      [(Stx (cons stx stl) ctx)
       (Stx (cons (flip stx scp) (flip-stl stl scp))
            (addremove scp ctx))]
      [(Stx (? atom? atom) ctx)
       (Stx atom (addremove scp ctx))]
      [(cons stx stl) (cons (flip stx scp) (flip-stl stl scp))])))

(define-compound-unit/infer syntax@
  (import terms^ terms-extra^)
  (export s)
  (link   (([ss : syntax^]) super:syntax@)
          (([s  : syntax^]) syntax/super@ ss)))
